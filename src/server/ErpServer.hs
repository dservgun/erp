
module ErpServer(serverMain, testServerMain)where
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

testServerMain :: MVar String -> FilePath -> IO()
testServerMain m dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do    
    putStrLn $ "Listening on " ++  (show portNumber)
    -- This is still not a very good signal..because
    -- we could have an exception in the runServer.
    -- But runServer is a development tool...not 
    -- suitable for production use
    putMVar m "Started..presumably"    
    WS.runServer "127.0.0.1" portNumber $ handleConnection m acid
    putStrLn "After the call to the handleConnection" 
  )

serverMain :: FilePath -> IO ()
serverMain dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do    
    TIO.putStrLn $ T.pack("Listening on " ++  (show portNumber))
    m <- newEmptyMVar
    WS.runServer "127.0.0.1" portNumber $ handleConnection m acid)

portNumber = 8082
{--
A simple echo.
--}
instance Show WS.Connection where
    show _  = "Connection info.....WS sockets should provide some defaults??"
handleConnection m acid pending = do
  os <- getEnv("os")  
  conn <- WS.acceptRequest pending
  TIO.putStrLn $ T.pack ("Accepted connection.." ++ show conn)
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
