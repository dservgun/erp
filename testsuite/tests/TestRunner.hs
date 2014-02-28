
import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L

import ErpServer(testServerMain)
import Control.Monad(forever, unless)
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Concurrent.Async(async, wait)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La


sendRequest aPayload = encode( toJSON (M.Request "Login" $ En.decodeUtf8 aPayload))
clientTest :: WS.ClientApp ()
clientTest conn = do
    T.putStrLn "Connected successfully"
    tR <- async( forever $ do 
        msg <- WS.receiveData conn
        T.putStrLn (msg :: Text)
        )    
    -- Send a verified user and an unverified user,
    -- the recovery should not be showing the unverified user.
    
    WS.sendTextData conn $ sendRequest (encode(toJSON (L.Login "test@test.org" True)))
    WS.sendClose conn ("Closing connection" :: Text)
    wait tR

    
main = do
    T.putStrLn "Starting server"
    m <- newEmptyMVar
    s <- async (testServerMain m "./dist/build/tests/state")
    T.putStrLn "Waiting for the server to start"
    T.putStrLn "Starting client thread"
    mvarValue <- takeMVar m
    T.putStrLn $ T.pack("Mvar returned " ++ show mvarValue)
    c <- async (WS.runClient "localhost" 8082 "/" clientTest)
    r <- wait s
    rc <- wait c
    T.putStrLn "Waiting for connections"
