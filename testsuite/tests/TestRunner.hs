
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


waitSome anInt = threadDelay $ anInt * 1000
sendRequest aPayload  = encode( toJSON (M.Request "Login" $ En.decodeUtf8 aPayload))

processResponse aRequest@(M.Request entity payload) = T.pack $ show aRequest
parseMessage :: T.Text -> IO ()
parseMessage aMessage = 
    let 
        r = J.decode $ En.encodeUtf8 $ La.fromStrict aMessage
    in 
        case r of         
        Just aRequest -> T.putStrLn $ processResponse aRequest
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
    
    WS.sendTextData conn $ sendRequest (encode(toJSON $ testLogin)) 
    -- WS.sendClose conn ("Closing connection" :: Text)
    wait tR

categoryTest :: String -> WS.ClientApp () 
categoryTest aString conn = 
    do
    tR <- async (forever $ do
            msg <- WS.receiveData conn
            parseMessage (msg :: Text)
            )
    WS.sendTextData conn $ sendRequest (encode (toJSON (Co.Category aString testLogin )))
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
    cat <- async(WS.runClient "localhost" 8080 "/" $ categoryTest "Test Category")
    rc <- wait c
    rCat <- wait cat
    T.putStrLn "End tests"
    cancel s
