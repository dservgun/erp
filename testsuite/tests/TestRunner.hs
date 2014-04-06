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
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.DateTime
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La
import qualified System.Directory as SD
import Test.QuickCheck

import Product as Pr
import Text.Printf
import TestHarness

testEmail = "test@test.org"
createQueryDatabaseRequest login aPayload =
    encode $ toJSON $ M.Request M.queryDatabaseConstant login $ En.decodeUtf8 aPayload

createLoginRequest login aPayload  = encode( toJSON (M.Request M.loginConstant login $ En.decodeUtf8 aPayload))

createCategoryRequest login aPayload = encode $ toJSON $ M.Request M.categoryConstant
        login $ En.decodeUtf8 aPayload
createCloseConnection login aPayload =
    encode $ toJSON $ M.Request M.closeConnection login $ En.decodeUtf8 aPayload


processResponse aRequest@(M.Request entity email payload) = T.pack $ show aRequest
parseMessage :: WS.Connection-> IO ()
parseMessage conn = do
    msg <- WS.receiveData conn
    let
        r = J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of
        Just aRequest ->
            case M.requestEntity aRequest of
                "CloseConnection" -> do
                    T.putStrLn "Received :: "
                    WS.sendClose conn ("Closing" :: T.Text)
                _ -> do
                    T.putStrLn "Received ::"
                    T.putStrLn $ processResponse aRequest

        _ -> throw M.InvalidRequest

testLogin = L.Login "test@test.org" True

loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    T.putStrLn "Client Connected successfully"
    tR <- async( parseMessage conn)
    -- Send a verified user and an unverified user,
    -- the recovery should not be showing the unverified user.
    WS.sendTextData conn $ createLoginRequest testEmail $ encode (toJSON testLogin)
    WS.sendTextData conn $ createCloseConnection testEmail $ encode (toJSON testLogin)
    wait tR

categoryTest :: String -> WS.ClientApp ()
categoryTest aString conn =
    do
    T.putStrLn "Connected successfully"
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createCategoryRequest testEmail $ encode $ toJSON $ Co.Category aString
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testEmail
    wait tR


databaseTest :: String -> WS.ClientApp ()
databaseTest aString conn =
    do
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createQueryDatabaseRequest testEmail $ encode . toJSON $ aString
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testEmail
    wait tR


serverTest = do
    T.putStrLn "Starting server"
    T.putStrLn "Removing acid state directory, from previous runs."
    SD.removeDirectoryRecursive acidStateTestDir
    m <- newEmptyMVar
    s <- async (testServerMain m acidStateTestDir)
    T.putStrLn "Waiting for the server to start"
    T.putStrLn "Starting client thread"
    mvarValue <- takeMVar m
    T.putStrLn $ T.pack("Mvar returned " ++ show mvarValue)
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2)
    cat <- async(WS.runClient "localhost" 8082 "/" $ categoryTest "Test Category")
    db <- async (WS.runClient "localhost" 8082 "/" $ databaseTest "Test query database")
    rc <- wait c
    rCat <- wait cat
    rdb <- wait db
    T.putStrLn "End tests"
    -- Cancel the server thread when all tests are done
    cancel s
    where
        acidStateTestDir = "./dist/build/tests/state"

main = do
    serverTest
    mapM_ (\(s, a) -> printf "%-25s" s >> a) ( tests)
prop1 = Pr.validUOM
tests = [("properties_tests" :: String, quickCheck prop1)
          , ("currency_valid" :: String, quickCheck prop_currency)
         ,("company_work_time" :: String, quickCheck prop_company_time),
         ("party_categories" :: String, quickCheck prop_party_categories),
         ("party_contacts" :: String, quickCheck prop_party_contacts),
         ("account_valid" :: String, quickCheck prop_valid_account),
         ("journal_valid" :: String, quickCheck prop_valid_journal)]
prop_currency  = Co.validCurrencies
prop_company_time  = Co.validHours

prop_party_categories  = Co.validCategories
prop_party_contacts  = Co.validContacts

prop_valid_account = Ac.validAccount
prop_valid_journal = Ac.validJournal
