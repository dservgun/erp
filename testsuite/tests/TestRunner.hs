
import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L
import qualified Data.Aeson as J
import qualified Company as Co
import ErpServer(testServerMain)
import Control.Monad(forever, unless)
import Control.Monad.Trans (liftIO)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async(async, wait, cancel)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La
import qualified System.Directory as SD


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
        Just aRequest -> do
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
    WS.sendTextData conn $ createLoginRequest testEmail (encode(toJSON $ testLogin)) 
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testLogin
    wait tR

categoryTest :: String -> WS.ClientApp () 
categoryTest aString conn = 
    do
    T.putStrLn "Connected successfully"
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createCategoryRequest testEmail(encode (toJSON (Co.Category aString)))
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testEmail
    wait tR

        
databaseTest :: String -> WS.ClientApp ()
databaseTest aString conn =
    do
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createQueryDatabaseRequest testEmail $ encode . toJSON $ aString
    wait tR
    
main = do
    T.putStrLn "Starting server"
    T.putStrLn $ "Removing acid state directory, from previous runs."
    SD.removeDirectoryRecursive acidStateTestDir
    m <- newEmptyMVar
    s <- async (testServerMain m acidStateTestDir)
    T.putStrLn "Waiting for the server to start"
    T.putStrLn "Starting client thread"
    mvarValue <- takeMVar m
    T.putStrLn $ T.pack("Mvar returned " ++ show mvarValue)
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2)
--    cat <- async(WS.runClient "localhost" 8082 "/" $ categoryTest "Test Category")
--    db <- async (WS.runClient "localhost" 8082 "/" $ databaseTest "Test query database")
    rc <- wait c
--    rCat <- wait cat
--    rdb <- wait db
    T.putStrLn "End tests"
    cancel s
    where
        acidStateTestDir = "./dist/build/tests/state"
