-- Declaring this as part of modules is breaking the tests??
--FIX this



import Control.Concurrent
import Control.Concurrent.Async(async, wait, cancel)
import Control.Exception
import Control.Monad(forever, unless)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.DateTime
import Data.Text (Text)
import Data.Time.Clock
import ErpError
import ErpServer (testServerMain, serverModuleName, IncomingRequestType(..))
import GHC.Generics
import Product as Pr
import ProductSpec
import qualified Account as Ac
import qualified Company as Co
import qualified Currency as Cu
import qualified Data.Aeson as J
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as La
import qualified Data.Text.Lazy.Encoding as En
import qualified ErpModel as M
import qualified Login as L
import qualified Network.WebSockets as WS
import qualified System.Directory as SD
import System.Log.Formatter
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler (setFormatter) 
import System.Log.Handler.Simple
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Handler.Syslog
import System.Log.Logger
import System.Log.Logger
import Test.Hspec
import Test.QuickCheck
import TestHarness
import Text.Printf


testEmail = "test@test.org"
createQueryDatabaseRequest anID login aPayload =
    encode $ toJSON $ M.Request  anID M.Retrieve
            M.protocolVersion
            (show QueryDatabase)
            login $ En.decodeUtf8 aPayload

createQueryNextSequenceRequest anID login payload = 
        encode $ toJSON $ M.Request anID M.Query
            M.protocolVersion
            (show QueryNextSequence )
            login $ En.decodeUtf8 payload

createLoginRequest anID login aPayload  = encode( toJSON (M.Request 
                    anID
                    M.Create
                    M.protocolVersion
                    (show Login) login
                    $ En.decodeUtf8 aPayload))


createCategoryRequest anID login aPayload = 
        encode $ toJSON $ M.Request 
        anID
        M.Create
        M.protocolVersion
        (show UpdateCategory)
        login $ En.decodeUtf8 aPayload

createCloseConnection anID login aPayload =
    encode $ toJSON $ M.Request anID M.Command 
            M.protocolVersion
            (show CloseConnection) login $ En.decodeUtf8 aPayload

endSession :: M.Response -> WS.Connection -> IO()
endSession aResponse conn = 
    let 
        nextSequence = M.getSequenceNumber aResponse in 
    do
        WS.sendTextData conn $  createCloseConnection nextSequence
                testEmail $ 
                encode $ toJSON testLogin      
    


clientStateMachine :: WS.Connection ->  M.Response -> IO ()
clientStateMachine conn aResponse = do
        responseEntity <- return $ M.getResponseEntity aResponse
        infoM testModuleName ("client state machine : Incoming response " ++ (show aResponse))
        nextSequenceNumber <- return $  M.getSequenceNumber aResponse
        if nextSequenceNumber == -1 then do
            infoM testModuleName ("Invalid sequence number " ++ (show aResponse))
            WS.sendClose conn  ("Invalid sequence number. Exiting" :: T.Text)
        else 
            case responseEntity of
                Just re ->
                    do
                        responseEntityType <- return $ read re
                        case responseEntityType of
                            Login -> do
                                    WS.sendTextData conn $ 
                                        createCategoryRequest nextSequenceNumber testEmail 
                                            $ encode $ toJSON $ Co.Category "Test category"
                                    parseLoginTestMessages conn    
                            UpdateCategory -> do 
                                    WS.sendTextData conn $ createQueryDatabaseRequest 
                                        nextSequenceNumber testEmail $ encode . toJSON $ 
                                        ("Test query database" :: String)
                                    parseLoginTestMessages conn
                            QueryDatabase -> do
                                endSession aResponse conn
                                parseLoginTestMessages conn
                            CloseConnection -> do
                                        debugM testModuleName $ "TestRunner: Received :: " ++ (show responseEntity)
                                        WS.sendClose conn  ("Bye." ::T.Text)
                Nothing -> do
                                    infoM testModuleName $ "Nothing : Received " ++ (show aResponse)
                                    WS.sendClose conn ("Unhandled command for this test case " 
                                                :: T.Text)                       


parseLoginTestMessages :: WS.Connection -> IO()
parseLoginTestMessages conn = do
    msg <- WS.receiveData conn
    r <- return $ J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of
        Just aResponse -> clientStateMachine conn aResponse
        Nothing -> do
                        debugM testModuleName $ "Unknown response. Quit here?"
                        WS.sendClose conn ("Unhandled command " :: T.Text)



testLogin = L.Login "test@test.org" True
testModuleName = "TestRunner" 

loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    debugM testModuleName "Client Connected successfully"
    tR <- async( parseLoginTestMessages conn)
    -- Send a verified user and an unverified user,
    -- Recovery should not be showing the unverified user.
    debugM testModuleName "Sending login request"
    WS.sendTextData conn $ createLoginRequest 1 testEmail $ encode $ toJSON testLogin
    wait tR
    debugM testModuleName "loginTest complete."


sampleCategoryMessages :: IO[Co.Category]
sampleCategoryMessages = sample' arbitrary


serverTest = do 
    updateGlobalLogger M.modelModuleName $ setLevel DEBUG  
    updateGlobalLogger testModuleName $ setLevel DEBUG 
    updateGlobalLogger ErpServer.serverModuleName $ setLevel DEBUG
    infoM testModuleName "Cleaning up past state."
    dirExists <- SD.doesDirectoryExist acidStateTestDir
    case dirExists of
        True -> SD.removeDirectoryRecursive acidStateTestDir
        False -> infoM testModuleName "Directory does not exist"
    m <- newEmptyMVar
    s <- async (testServerMain m acidStateTestDir)


    infoM testModuleName "SERVER started"
    mvarValue <- takeMVar m
    infoM testModuleName "SERVER ready"
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2)
    rc <- wait c
    infoM testModuleName "End tests"
    -- Cancel the server thread when all tests are done
    cancel s
    return ()
    where
        acidStateTestDir = "./dist/build/tests/state"

main = serverTest
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
    ErpError.Success aCom -> Co.validHours aCom 2000
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
