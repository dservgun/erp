
import ErpModel
import ErpServer(serverMain)
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
	T.putStrLn "Client Connected"
	tR <- async( forever $ do 
		msg <- WS.receiveData conn
		T.putStr ("Client received...")
		T.putStrLn (msg :: Text)
		)    
	WS.sendTextData conn (encode(toJSON (Login "test@test.org" True)))
	WS.sendTextData conn (encode(toJSON (Login "testu@test.org" False)))

	WS.sendClose conn ("Closing connection" :: Text)
	wait tR

waitSome :: IO ()
waitSome = threadDelay $ 200 * 1000
	
main = do
	T.putStrLn ("Starting server")
	s <- async (serverMain "./dist/build/tests/state")
	T.putStrLn("Waiting for the server to start")
	waitSome 
	T.putStrLn ("Starting client thread")
	c <- async (WS.runClient "localhost" 8082 "/" clientTest)
	r <- wait s
	rc <- wait c
	T.putStrLn("Waiting for connections")
