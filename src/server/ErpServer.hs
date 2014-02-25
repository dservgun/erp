
module ErpServer(serverMain)where
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Monad (forever)
import Control.Exception(bracket, handle, fromException)
import Control.Concurrent
import Control.Concurrent.Async(async, wait)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import System.Environment(getEnv)
import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L
import GHC.Generics
import Data.Aeson

serverMain :: FilePath -> IO ()
serverMain dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do
    putStrLn $ "Listening on " ++  (show portNumber)
    WS.runServer "127.0.0.1" portNumber $ handleConnection acid)

portNumber = 8082
{--
A simple echo.
--}

handleConnection acid pending = do
  os <- getEnv("os")
  TIO.putStrLn(T.pack("Starting server on. - ."  ++ os))
  conn <- WS.acceptRequest pending
  TIO.putStrLn("Accepted connection")
  sendHistory conn acid
  a1 <-   async (echo conn acid)
  TIO.putStrLn ("Waiting for this thread to finish")
  r <- wait a1
  TIO.putStrLn("Handling connection requests ...")


sendHistory conn acid =
  do
    messages <- L.getHistory acid 100
    mapM_ (\m -> 
		do 
			TIO.putStrLn(T.pack $"Server sending key " ++ m)
			WS.sendTextData conn (T.pack m)) messages

echo conn acid =
     handle catchDisconnect  $ forever $ do
     msg <- WS.receiveData conn
     TIO.putStrLn(msg)	 
     L.upsertEmail acid (msg)
     WS.sendTextData conn msg
     where	   
       catchDisconnect e =
         case fromException e of
           Just  WS.ConnectionClosed ->
                 do
                        TIO.putStrLn("Connection closed ")
                        return ()
           _ -> do
            TIO.putStrLn (T.pack ("Unknown exception " ++ show e))
            return ()
