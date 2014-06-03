import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L
import qualified Data.Aeson as J
import qualified Company as Co
import qualified Currency as Cu
import qualified Account as Ac

import ErpServer(testServerMain)
import Control.Monad(forever, unless)
import Control.Monad.Trans (liftIO)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async(async, wait, cancel)
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Data.DateTime
import qualified Network.WebSockets as WS
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter) 
import System.Log.Formatter

import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La
import qualified System.Directory as SD
import Test.QuickCheck
import ErpError
import Product as Pr
import Text.Printf
import TestHarness
import Test.Hspec
import ProductSpec
testEmail = "test@test.org"
createQueryDatabaseRequest anID login aPayload =
    encode $ toJSON $ M.Request  anID
            M.protocolVersion
            M.queryDatabaseConstant login $ En.decodeUtf8 aPayload

createQueryNextSequenceRequest anID login payload = 
        encode $ toJSON $ M.Request anID
            M.protocolVersion
            M.queryNextSequenceConstant 
            login $ En.decodeUtf8 payload

createLoginRequest anID login aPayload  = encode( toJSON (M.Request 
                    anID
                    M.protocolVersion
                    M.addLoginConstant login
                    $ En.decodeUtf8 aPayload))

createCategoryRequest anID login aPayload = 
        encode $ toJSON $ M.Request 
        anID
        M.protocolVersion
        M.updateCategoryConstant
        login $ En.decodeUtf8 aPayload

createCloseConnection anID login aPayload =
    encode $ toJSON $ M.Request anID M.protocolVersion
            M.closeConnectionConstant login $ En.decodeUtf8 aPayload


processResponse aRequest@(M.Request anID aVersion entity email payload) = T.pack $ show aRequest
parseMessage :: WS.Connection-> IO ()
parseMessage conn = do
    msg <- WS.receiveData conn
    let
        r = J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of
        Just aRequest ->
            case M.requestEntity aRequest of
                "CloseConnection" -> do
                    TIO.putStrLn "Received :: "
                    WS.sendClose conn ("Closing" :: T.Text)
                _ -> do
                    TIO.putStrLn $ T.pack ("Received ::" ++ (show aRequest))
                    TIO.putStrLn $ processResponse aRequest

        _ -> do
            TIO.putStrLn $ T.pack("Invalid Request " ++ (show r))
            throw M.InvalidRequest


testLogin = L.Login "test@test.org" True


loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    TIO.putStrLn "Client Connected successfully"
    tR <- async( parseMessage conn)
    -- Send a verified user and an unverified user,
    -- Recovery should not be showing the unverified user.
    WS.sendTextData conn $ createLoginRequest 1 testEmail $ encode $ toJSON testLogin
    WS.sendTextData conn $ createQueryNextSequenceRequest  M.errorID testEmail $ encode (toJSON testLogin)
    -- WS.sendTextData conn $ createCloseConnection 2 testEmail $ encode (toJSON testLogin)
    wait tR

categoryTest :: String -> WS.ClientApp ()
categoryTest aString conn =
    do
    TIO.putStrLn "Connected successfully"

    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createCategoryRequest 1 testEmail $ encode $ toJSON $ Co.Category aString
    -- WS.sendTextData conn $ createCloseConnection 2 testEmail $ encode $ toJSON testEmail
    wait tR


databaseTest :: String -> WS.ClientApp ()
databaseTest aString conn =
    do
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createQueryDatabaseRequest 1 testEmail $ encode . toJSON $ aString
    --WS.sendTextData conn $ createCloseConnection 2 testEmail $ encode $ toJSON testEmail
    wait tR


serverTest = do
    TIO.putStrLn "Cleanup."
    dirExists <- SD.doesDirectoryExist acidStateTestDir
    case dirExists of
        True -> SD.removeDirectoryRecursive acidStateTestDir
        False -> TIO.putStrLn "."
    m <- newEmptyMVar
    s <- async (testServerMain m acidStateTestDir)
    TIO.putStrLn "Starting server"

    TIO.putStrLn "Starting..."
    mvarValue <- takeMVar m
    TIO.putStrLn "Server ready."
    TIO.putStrLn "Starting client thread"
    TIO.putStrLn $ T.pack("Mvar returned " ++ show mvarValue)
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2)
--   cat <- async(WS.runClient "localhost" 8082 "/" $ categoryTest "Test Category")
--  db <- async (WS.runClient "localhost" 8082 "/" $ databaseTest "Test query database")
    rc <- wait c
  --  rCat <- wait cat
--    rdb <- wait db
    TIO.putStrLn "End tests"
    -- Cancel the server thread when all tests are done
    cancel s
    where
        acidStateTestDir = "./dist/build/tests/state"

main = do
    updateGlobalLogger ""  $ setLevel DEBUG
    serverTest
    mapM_ (\(s, a) -> printf "%-25s" s >> a) ( tests)
    hspec $ describe "ProductSpec" ProductSpec.spec






tests = [
         ("properties_tests" :: String, quickCheck prop1)
         , ("currency_valid" :: String, quickCheck prop_currency)
         , ("company_work_time" :: String, quickCheck prop_company_time)
         , ("party_categories" :: String, quickCheck prop_party_categories)
         , ("party_contacts" :: String, quickCheck prop_party_contacts)
         , ("account_valid" :: String, quickCheck prop_valid_account)
         , ("journal_valid" :: String, quickCheck prop_valid_journal)
         , ("dimensions_valid" :: String, quickCheck prop_valid_dimensions)
         ]

prop_currency :: ErpError ModuleError Co.Company -> Bool
prop_currency (ErpError.Success aCompany) = Co.validCurrencies aCompany
-- I need a better way to express this.
prop_currency (ErpError.Error _) = True

prop_company_time :: ErpError ModuleError Co.CompanyWorkTime -> Bool
prop_company_time a =
    case a of
    ErpError.Success aCom -> Co.validHours aCom
    ErpError.Error _ -> True

prop_party_categories :: ErpError ModuleError Co.Party -> Bool
prop_party_categories a =
    case a of
    ErpError.Success cat -> Co.validCategories cat
    ErpError.Error _ -> True

prop_party_contacts :: ErpError ModuleError Co.Party -> Bool
prop_party_contacts a =
    case a of
    ErpError.Success con -> Co.validContacts con
    ErpError.Error  _ -> True


prop_valid_account a =
    case a of
        ErpError.Error _ -> True
        _          -> Ac.validAccount a

prop_valid_journal :: ErpError ModuleError Ac.Journal -> Bool
prop_valid_journal (ErpError.Success aJournal )= Ac.validJournal aJournal
prop_valid_journal (ErpError.Error _ ) = True



prop1 :: ErpError ModuleError Pr.UOM -> Bool
prop1 aValue =
    case aValue of
    ErpError.Error _ -> True
    ErpError.Success aUOM -> Pr.validUOM aUOM

prop_valid_dimensions :: ErpError.ErpError ModuleError Dimensions -> Bool
prop_valid_dimensions aValue =
    case aValue of
    ErpError.Error _ -> True
    ErpError.Success aDim -> Pr.validDimensions aDim
