
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



testEmail = "test@test.org"
sendQueryDatabaseRequest aPayload = 
    encode $ toJSON $ M.Request "QueryDatabase" testEmail $ En.decodeUtf8 aPayload
sendLoginRequest aPayload  = encode( toJSON (M.Request "Login" testEmail $ En.decodeUtf8 aPayload))
sendCategoryRequest aPayload = encode $ toJSON $ M.Request "Category" testEmail $ En.decodeUtf8 aPayload

processResponse aRequest@(M.Request entity email payload) = T.pack $ show aRequest
parseMessage :: T.Text -> IO ()
parseMessage aMessage = 
    let 
        r = J.decode $ En.encodeUtf8 $ La.fromStrict aMessage
    in 
        case r of         
        Just aRequest -> do
            T.putStrLn "Received :: "
            T.putStrLn $ processResponse aRequest
        _ -> throw M.InvalidRequest

testLogin = L.Login "test@test.org" True        

loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    T.putStrLn "Connected successfully"
    tR <- async( forever $ do 
        msg <- WS.receiveData conn
        parseMessage (msg :: Text)
        )    
    -- Send a verified user and an unverified user,
    -- the recovery should not be showing the unverified user.
    
    WS.sendTextData conn $ sendLoginRequest (encode(toJSON $ testLogin)) 
    -- WS.sendClose conn ("Closing connection" :: Text)
    wait tR

categoryTest :: String -> WS.ClientApp () 
categoryTest aString conn = 
    do
    T.putStrLn "Connected successfully"
    tR <- async (forever $ do
            msg <- WS.receiveData conn
            parseMessage (msg :: Text)
            )
    WS.sendTextData conn $ sendCategoryRequest (encode (toJSON (Co.Category aString)))
    wait tR
databaseTest :: String -> WS.ClientApp ()
databaseTest aString conn =
    do
    tR <- async(forever $ do
                msg <- WS.receiveData conn
                parseMessage (msg :: Text))
    WS.sendTextData conn $ sendQueryDatabaseRequest $ encode . toJSON $ aString
    wait tR
    
main = do
    T.putStrLn "Starting server"
    m <- newEmptyMVar
    s <- async (testServerMain m "./dist/build/tests/state")
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
    cancel s
