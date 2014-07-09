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
import qualified SystemSequence as SSeq
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
import ErpServer

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


conversationTest :: WS.Connection -> IO()
conversationTest conn = do
    msg <- WS.receiveData conn
    r <- return $ J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of
        Just res -> do
            nextSequenceNumber <- return $ M.getSequenceNumber res
            sCat <- sampleCategoryMessages
            infoM testModuleName "Conversation test"
            conversation conn sCat res
        Nothing -> return ()


testLogin = L.Login "test@test.org" True
testModuleName = "TestRunner" 

loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    debugM testModuleName "Client Connected successfully"
    -- tR <- async( parseLoginTestMessages conn)
    tR <- async(conversationTest conn)
    -- Send a verified user and an unverified user,
    -- Recovery should not be showing the unverified user.
    debugM testModuleName "Sending login request"
    WS.sendTextData conn $ createLoginRequest 1 testEmail $ encode $ toJSON testLogin
    wait tR
    debugM testModuleName "loginTest complete."


sampleCategoryMessages :: IO[M.Request]
sampleCategoryMessages = do
        s <- sample' arbitrary
        mapM (\x -> return $ createCategoryRequest1 1 testEmail x) s



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
