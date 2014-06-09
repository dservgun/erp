
module ErpServer(serverMain, testServerMain)where
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Monad (forever)
import Control.Exception(bracket, handle, fromException)
import Control.Concurrent
import Control.Concurrent.Async(async, wait)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Acid as A
import qualified Network.WebSockets as WS
import System.Environment(getEnv)
import Data.Aeson as J
import GHC.Generics
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO

import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import qualified ErpModel as M
import qualified Login as Lo
import qualified Company as Co


data InvalidRequest = InvalidRequest deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidLogin = InvalidLogin deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidCategory = InvalidCategory deriving (Show, Generic, Typeable, Eq, Ord)


testServerMain :: MVar String -> FilePath -> IO()
testServerMain m dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do
    putStrLn $ "Listening on " ++  show portNumber
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
    TIO.putStrLn $ T.pack("Listening on " ++  show portNumber)
    m <- newEmptyMVar
    WS.runServer "127.0.0.1" portNumber $ handleConnection m acid)

portNumber = 8082

instance Show WS.Connection where
    show _  = "Connection info...TBD"
    
handleConnection m acid pending = do
  conn <- WS.acceptRequest pending
  TIO.putStrLn $ T.pack ("Accepted connection.." ++ show conn)
  a1 <-   async (processMessages conn acid)
  r <- wait a1
  TIO.putStrLn "Handling connection requests ..."

serverModuleName = "ErpServer"
processMessages conn acid =
     handle catchDisconnect  $ forever $ do
     msg <- WS.receiveData conn
     debugM serverModuleName $ "Processing " ++ (show msg)
     updateDatabase conn acid msg
     where
       catchDisconnect e =
         case fromException e of
           Just  WS.ConnectionClosed ->
                 do
                        infoM serverModuleName  "Connection closed "
                        return ()
           _ -> do
            errorM serverModuleName $ serverModuleName ++ " catchDisconnect:: Unknown exception " ++ show e
            return ()

updateDatabase connection acid aMessage =
    let
        r = J.decode $ E.encodeUtf8 $ L.fromStrict aMessage
    in
        case r of
        Just aRequest -> do
                processRequest connection acid aRequest
                postProcessRequest connection acid aRequest
        _ -> throw InvalidRequest

postProcessRequest connection acid r = do
  nextSequenceResponse <- sendNextSequence acid  r
  case nextSequenceResponse of 
    Just x -> WS.sendTextData connection  $ J.encode x
    Nothing -> M.sendError connection r "Add login request failed"

processRequest connection acid r@(M.Request iRequestID 
        iProtocolVersion entity emailId payload)  =
    if iProtocolVersion /= M.protocolVersion then
        do
        debugM M.moduleName $  "Invalid protocol message " ++ iProtocolVersion
        M.sendError connection r $ 
          L.pack ("Invalid protocol version : " ++ M.protocolVersion)
    else 
        do
            debugM M.moduleName $ "Incoming request " ++ (show r)
            case entity of
                "QueryNextSequence"-> debugM M.moduleName $ "Processing message  " ++ (show entity)
                "Login" -> updateLogin acid r
                "DeleteLogin" -> deleteLoginA acid emailId
                "UpdateCategory" -> updateCategory acid emailId $ L.toStrict payload
                "QueryDatabase"  ->do
                     model <- queryDatabase acid emailId $ L.toStrict payload 
                     TIO.putStrLn $ T.pack $ show model                       
                "CloseConnection" -> 
                            let 
                                response = M.createCloseConnectionResponse r 
                            in 
                            do
                            debugM M.moduleName $ "ErpModel::Sending " ++ (show 
                                    response)                            
                            WS.sendTextData connection $ J.encode response
                _ -> do
                            errorM M.moduleName $ "Invalid request received " ++ (show r)
                         

deleteLoginA acid anEmailId = A.update acid (M.DeleteLogin anEmailId)

getEmail payload = 
    let
        pObject = pJSON payload
    in
        case pObject of
            Just (Lo.Login email verified) -> email
            Nothing -> throw InvalidLogin

--Authentication is probably done using an oauth provider
--such as persona or google. This method simply logs
--in the user as valid.
updateLogin acid r =
     let
        payload = L.toStrict (M.requestPayload r)
        pObject = pJSON  $ L.toStrict (M.requestPayload $ r)
     in
        case pObject of
            Just l@(Lo.Login name email) -> do
                    loginLookup <- A.query acid (M.LookupLogin name)
                    case loginLookup of
                        Nothing -> do 
                                        A.update acid (M.InsertLogin name r l)
                        Just l2@(Lo.Login name email) ->
                            return ()
            Nothing -> throw InvalidLogin

sendNextSequence acid request = 
    let
        emailId = M.getRequestEmail request 
    in
        do
        lookup <- A.query acid (M.GetDatabase emailId)
        case lookup of 
            Nothing -> 
                do
                    let 
                        res = M.createNextSequenceResponse emailId (Just request)
                                 $ M.errorID
                    debugM M.moduleName $ "Could not find database " ++ (show res)
                    return $ Just res 
            Just x -> 
                do
                    let 
                        res = M.createNextSequenceResponse emailId ( Just request) 
                                $ (M.nextRequestID  x)
                    debugM M.moduleName $ "Database found " ++ (show res)
                    debugM M.moduleName $ show res
                    return $ Just res


queryNextSequence acid request = 
    let 
        emailId = M.getRequestEmail request
    in 
        do
            debugM M.moduleName $ "Querying for " ++ emailId
            lookup <- A.query acid (M.GetDatabase emailId)
            debugM M.moduleName $ "Lookup " ++ (show lookup)
            case lookup of
                Nothing -> return Nothing
                Just l -> return $ Just $ M.createNextSequenceResponse 
                                emailId Nothing
                                 (M.nextRequestID l) 

updateCategory acid emailId payload =
    let
        pObject = pJSON payload
    in
        case pObject of
            Just c@(Co.Category aCat) -> do
               -- infoM "ErpModel" "Processing update category"
                lookup <- A.query acid (M.LookupCategory emailId c)
                case lookup of
                    Nothing -> A.update acid (M.InsertCategory emailId c)
                    Just c@(Co.Category aCat) -> return ()
            Nothing -> return ()
queryDatabase acid emailId payload = do
    debugM  M.moduleName $ "Querying database " ++ emailId
    lookup <- A.query acid (M.GetDatabase emailId)
    debugM M.moduleName $ "Query returned " ++ (show lookup)
    return lookup

displayText = T.pack . show
pJSON = J.decode . E.encodeUtf8 . L.fromStrict


instance Exception InvalidCategory
instance Exception InvalidLogin
instance Exception InvalidRequest
instance J.ToJSON InvalidLogin
instance J.FromJSON InvalidLogin
