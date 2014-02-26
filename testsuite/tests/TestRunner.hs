
import ErpModel
import Login
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


clientTest :: WS.ClientApp ()
clientTest conn = do
    T.putStrLn "Client Connected...\n"
    tR <- async( forever $ do 
        msg <- WS.receiveData conn
        T.putStr ("Client received...\n")
        T.putStrLn (msg :: Text)
        )    
    WS.sendTextData conn (encode(toJSON (Login "test@test.org" True)))
    WS.sendTextData conn (encode(toJSON (Login "testu@test.org" False)))
    WS.sendClose conn ("Closing connection" :: Text)
    wait tR

    
main = do
    T.putStrLn ("Starting server")
    m <- newEmptyMVar
    s <- async (testServerMain m "./dist/build/tests/state")
    T.putStrLn("Waiting for the server to start")
    T.putStrLn ("Starting client thread")
    mvarValue <- takeMVar m
    T.putStrLn $ T.pack("Mvar returned " ++ (show mvarValue))
    c <- async (WS.runClient "localhost" 8082 "/" clientTest)
    c2 <- async (WS.runClient "localhost" 8082 "/" clientTest)
    r <- wait s
    rc <- wait c
    rc2 <- wait c2
    T.putStrLn("Waiting for connections")
