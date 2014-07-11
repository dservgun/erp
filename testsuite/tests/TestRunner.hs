-- Declaring this as part of modules is breaking the tests??
--FIX this



import Control.Concurrent
import Control.Concurrent.Async(async, wait, cancel)
import Control.Exception
import Control.Monad(forever, unless, liftM)

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
import qualified SystemSequence as SSeq
import qualified System.Directory as SD
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog
import System.Log.Logger
import Test.Hspec
import Test.QuickCheck
import TestHarness
import Text.Printf
import ErpServer

testEmail = "test@test.org"
createQueryDatabaseRequest anID login aPayload =
    encode $ toJSON $ M.Request  anID M.Retrieve
            M.protocolVersion
            (show QueryDatabase)
            login $ En.decodeUtf8 aPayload

createQueryNextSequenceRequest anID login payload = 
        M.Request anID M.Query
            M.protocolVersion
            (show QueryNextSequence )
            login $ En.decodeUtf8 payload

createLoginRequestObj anID login aPayload  = M.Request 
                    anID
                    M.Create
                    M.protocolVersion
                    (show Login) login
                    $ En.decodeUtf8 aPayload

createLoginRequest anID login aPayload  = encode $ toJSON $ createLoginRequestObj anID login aPayload


createInsertParty :: SSeq.ID -> String -> ErpError ModuleError Co.Party -> M.Request
createInsertParty anID login (ErpError.Success a) = 
            M.Request 
                anID
                M.Create
                M.protocolVersion
                (show InsertParty)
                login $ En.decodeUtf8 $ encode $ toJSON $ a

createInsertParty anID login (ErpError.Error b) = 
        -- Error cases should generate some no-op requests.
                M.Request 
                    anID
                    M.Create
                    M.protocolVersion
                    (show QueryDatabase) 
                    login $ En.decodeUtf8 $ encode $ toJSON $ (show b)



createCategoryRequest1 :: SSeq.ID -> String -> Co.Category -> M.Request
createCategoryRequest1 anID login aCategory =
    M.Request
        anID
        M.Create
        M.protocolVersion 
        (show UpdateCategory)
        login $ En.decodeUtf8 $ encode $ toJSON $ aCategory

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

-- Send a close request so the server can close the session. 
-- Wait for the response and then disconnect.
endSession :: M.Response -> WS.Connection -> IO()
endSession aResponse conn = 
    do
        nextSequence <- return $ M.getSequenceNumber aResponse
        infoM testModuleName "Ending session"
        WS.sendTextData conn $  createCloseConnection nextSequence
                testEmail $ 
                encode $ toJSON testLogin
        msg <- (WS.receiveData conn :: IO T.Text)
        WS.sendClose conn ("Ending session" :: T.Text)



conversation :: WS.Connection -> [M.Request] -> M.Response -> IO ()
conversation conn [] aResponse = endSession aResponse conn
conversation conn (h:t) aResponse = do
                WS.sendTextData conn $ encode $ toJSON (M.updateSequenceNumber aResponse h)
                msg <- WS.receiveData conn
                r <- return $ J.decode $ En.encodeUtf8 $ La.fromStrict msg
                case r of 
                    Just x -> conversation conn t x
                    Nothing -> endSession aResponse conn


startSession :: WS.Connection -> M.Request -> IO()
startSession conn aMessage = WS.sendTextData conn $ createLoginRequest 1 testEmail 
            $ encode $ toJSON testLogin

conversationTest :: WS.Connection -> M.Request -> IO [M.Request]-> IO()
conversationTest conn header messages = do
    WS.sendTextData conn $ encode $ toJSON header
    msg <- WS.receiveData conn
    r <- return $ J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of
        Just res -> do
            nextSequenceNumber <- return $ M.getSequenceNumber res
            infoM testModuleName "Conversation test"
            input <- messages
            conversation conn input res
        Nothing -> return ()


testLogin = L.Login "test@test.org" True
testModuleName = "TestRunner" 

loginTest :: Int -> M.Request -> IO [M.Request] -> WS.ClientApp ()
loginTest aVer header messages conn  = do
    debugM testModuleName "Client Connected successfully"
    tR <- async(conversationTest conn header messages)
    -- Send a verified user and an unverified user,
    -- Recovery should not be showing the unverified user.
    wait tR
    debugM testModuleName "Test complete."


sampleCategoryMessages :: IO[M.Request]
sampleCategoryMessages = do
        s <- sample' arbitrary
        mapM (\x -> return $ createCategoryRequest1 1 testEmail x) s


sampleInsertPartyMessages ::  IO[M.Request]
sampleInsertPartyMessages = do
    s <- (sample' $ (suchThat arbitrary (\a -> 
            case a of 
                ErpError.Success b -> True
                ErpError.Error c -> False
        ))):: IO [ErpError ModuleError Co.Party]
    mapM (\x -> return $ createInsertParty 1 testEmail x) s



serverTest = do 
    h <- fileHandler "debug.log" DEBUG 
    lh <- return $ setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger M.modelModuleName $ setLevel DEBUG . setHandlers[lh]
    updateGlobalLogger testModuleName $ setLevel DEBUG . setHandlers[h]
    updateGlobalLogger ErpServer.serverModuleName $ setLevel DEBUG . setHandlers[h]

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

    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2 login sampleCategoryMessages)
    rc <- wait c
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2 login sampleInsertPartyMessages)
    rc <- wait c
    infoM testModuleName "End tests"
    -- Cancel the server thread when all tests are done
    cancel s
    return ()
    where
        acidStateTestDir = "./dist/build/tests/state"
        header = createQueryNextSequenceRequest (-1) testEmail $ encode $ toJSON testLogin
        login = createLoginRequestObj 1 testEmail $ encode $ toJSON testLogin

main = serverTest
