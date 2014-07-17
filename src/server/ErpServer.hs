--module ErpServer(serverMain, testServerMain, serverModuleName
--    , handleConnection
--    , routeRequest
--    , IncomingRequestType(..))where
module ErpServer where
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
import qualified SystemSequence as SSeq
import qualified ErpModel as M
import qualified ErpError as ErEr
import qualified Login as Lo
import qualified Company as Co
import qualified IncomingRequestType as IR 


displayText = T.pack . show
pJSON = J.decode . E.encodeUtf8 . L.fromStrict


data InvalidRequest = InvalidRequest deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidLogin = InvalidLogin deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidCategory = InvalidCategory deriving (Show, Generic, Typeable, Eq, Ord)



testServerMain :: MVar String -> FilePath -> IO()
testServerMain m dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do
    infoM serverModuleName $ "Listening on " ++  show portNumber
    -- This is still not a very good signal..because
    -- we could have an exception in the runServer.
    -- But runServer is a development tool...not
    -- suitable for production use
    putMVar m "Started..presumably"
    WS.runServer "127.0.0.1" portNumber $ handleConnection acid
    infoM serverModuleName "After the call to the handleConnection"
  )

serverMain :: FilePath -> IO ()
serverMain dbLocation =
  bracket
  (M.initializeDatabase dbLocation)
  M.disconnect
  (\acid -> do
    infoM serverModuleName $ "Listening on " ++ show portNumber
    WS.runServer "127.0.0.1" portNumber $ handleConnection acid)

portNumber = 8082

instance Show WS.Connection where
    show _  = "Connection info...TBD"

handleConnection acid pending = do
  conn <- WS.acceptRequest pending
  infoM serverModuleName $ "Accepted connection " ++ show conn

  a1 <-   async (processMessages conn acid)
  r <- wait a1
  infoM serverModuleName "Handling connections..."

serverModuleName = "ErpServer"

processMessages conn acid =
     handle catchDisconnect  $ forever $ do

     msg <- WS.receiveData conn
     infoM serverModuleName $ "Process messages " ++ (show msg)
     updateDatabase conn acid msg
     where
       catchDisconnect e =
         case fromException e of
           Just  WS.ConnectionClosed ->
                 do
                    infoM serverModuleName "Closing connections."
                    return ()
           _ -> do
              errorM serverModuleName $ serverModuleName ++ " catchDisconnect:: Unknown exception " ++ show e
              return ()

updateDatabase connection acid aMessage = 
    do
        r <- return $ pJSON aMessage
        case r of
          Just aRequest -> do
                  response <- processRequest connection acid aRequest
                  updateRequests acid aRequest
                  postProcessRequest connection acid aRequest response
          _ -> do
                errorM serverModuleName (show r)
                M.sendError connection Nothing "Error"
                return $ ErEr.createErrorS "ErpServer" "ES002" $ "Invalid message " ++ (show aMessage)

updateRequests acid r = A.update acid(M.InsertRequest r)


postProcessRequest connection acid r erpResponse = do
  nextSequenceResponse <- sendNextSequence acid  r  
  case nextSequenceResponse of
    Just x -> 
              do
                A.update acid (M.InsertResponse x)
                M.sendMessage connection (M.updateResponseID x erpResponse)
                return erpResponse
    Nothing -> do
        moduleError <- return $ ErEr.createErrorS "ErpServer" "ES001" $ "Invalid response " ++ show r
        M.sendMessage connection moduleError
        return moduleError


payload = M.requestPayload

routeRequest :: WS.Connection -> AcidState(EventState M.QueryLogin) -> IR.IncomingRequestType -> M.Request -> 
  IO (ErEr.ErpError ErEr.ModuleError M.Response)
routeRequest connection acid IR.QueryNextSequence r = 
  do
    response <- sendNextSequence acid r
    case response of
      Just res -> return $ ErEr.createSuccess res
      Nothing -> return $ ErEr.createErrorS "ErpServer" "ES013" ("Command failed " 
        ++ show IR.QueryNextSequence)

routeRequest connection acid IR.Login  r            = 
    do
      updateLogin acid r
      response <- sendNextSequence acid r
      case response of 
        Just res -> return $ ErEr.createSuccess res
        Nothing -> return $ ErEr.createErrorS "ErpServer" "ES011" "Invalid response"

routeRequest connection acid IR.DeleteLogin r       = 
    do
      deleteLoginA acid $ M.emailId r
      response <- sendNextSequence acid r
      case response of
        Just res -> return $ ErEr.createSuccess res
        Nothing -> return $ ErEr.createErrorS "ErpServer" "ES010" "Invalid response"


routeRequest connection acid IR.UpdateCategory r    = 
  do
    updateCategory acid (M.emailId r) (L.toStrict (payload r))
    response <- sendNextSequence acid r
    case response of
      Just res -> return $ ErEr.createSuccess res
      Nothing -> return $ ErEr.createErrorS "ErpServer" "ES011" "Invalid Response"

routeRequest connection acid IR.InsertParty r = do
    insertParty acid (M.emailId r) (L.toStrict $ payload r)
    response <- sendNextSequence acid r
    case response of
      Just res -> return $ ErEr.createSuccess res
      Nothing -> return $ ErEr.createErrorS "ERPServer" "ES012" ("Invalid response " ++ (show IR.InsertParty))
routeRequest connection acid IR.InsertCompany r = do
    debugM serverModuleName $ "Insert  company" ++ (show r)
    insertCompany acid (M.emailId r) (L.toStrict $ payload r)
    response <- sendNextSequence acid r
    case response of
      Just res -> return $ ErEr.createSuccess res
      Nothing -> return $ ErEr.createErrorS "ERPServer" "ES012" ("Invalid response " ++ (show IR.InsertCompany))


routeRequest connection acid IR.QueryDatabase r    = do
  model <- queryDatabase acid (M.emailId r) $ L.toStrict (payload r)
  --log the model here if needed.
  response <- sendNextSequence acid r
  case response of
    Just res -> return $ ErEr.createSuccess res
    Nothing -> return $ ErEr.createErrorS "ErpServer" "ES012" "Invalid response"

routeRequest connection acid IR.CloseConnection  r = do
        -- Need to investigate how this following line is working
        response <- sendNextSequence acid r
        case response of
          Just res -> return $ ErEr.createSuccess res
          Nothing -> return $ ErEr.createErrorS "ErpServer" "ES013" "Close connection failed"
        

checkProtocol :: String -> ErEr.ErpError ErEr.ModuleError String
checkProtocol iProtocolVersion = -- Returns a ErpError if wrong protocol
    if iProtocolVersion == M.protocolVersion then ErEr.createSuccess "Protocol supported" -- TODO : ErpError
    else ErEr.createErrorS "ErpServer" "ES003" "Unsupported protocol"


processRequest :: WS.Connection -> AcidState (EventState M.GetDatabase) -> M.Request -> 
  IO (ErEr.ErpError ErEr.ModuleError M.Response)
processRequest connection acid r@(M.Request iRequestID requestType iProtocolVersion entity emailId payload) =
    --TODO: get the process entity so we are inside our own monad instead of IO
    case (checkProtocol iProtocolVersion) of
        ErEr.Error aString -> return $ ErEr.createErrorS "ErpServer" "ES001" "Check protocol failed"
        ErEr.Success aString -> 
                do
                  entityType <- return $ read entity
                  debugM M.modelModuleName $ "Incoming request " ++ (show r)
                  currentRequest <- checkRequest acid r  --TODO: Need to decode type better than a bool
                  if currentRequest then
                      do
                        response <- routeRequest connection acid entityType r
                        infoM serverModuleName ("Entity type " ++ (show entityType))
                        infoM serverModuleName ("Sending response--erperror" ++ (show response))
                        return response
                  else
                    do
                      moduleError <- return $ 
                          ErEr.createErrorS "ErpServer" "ES002" $ "Stale message " ++ show r
                      infoM serverModuleName  $ L.unpack (ErEr.getString moduleError)
                      return moduleError


deleteLoginA acid anEmailId = A.update acid (M.DeleteLogin anEmailId)

getEmail payload = do
        pObject <- return $ pJSON payload
        case pObject of
            Just (Lo.Login email verified) -> email
            Nothing -> throw InvalidLogin


-- The return should be more than a boolean.
-- When no model is found, we would like the caller
-- to create an empty model.
-- Returning true doesnt capture  this.

checkRequest acid r@(M.Request iRequestID requestType
    iProtocolVersion entity emailId payload) =
    do
      infoM serverModuleName ("checkRequest " ++ (show r))
      erp <- A.query acid (M.GetDatabase emailId)
      incomingRequestType <- return $ read entity
      case erp of
        Nothing ->  return True
        Just x -> 
          do
            infoM serverModuleName (" Input requestid " ++ (show iRequestID))
            if incomingRequestType /= IR.Login then
                return $  ( M.nextRequestID x r ) == iRequestID
            else
                return True -- Login requests dont know the id??


--Authentication is probably done using an oauth provider
--such as persona or google. This method simply logs
--in the user as valid. If the user is already registered,
-- nothing needs to be done. Else,
-- the user is verified and a new erp model is associated
-- with this email id. tl;dr

updateLogin acid r =
     let
        payload = L.toStrict (M.requestPayload r)
        pObject = pJSON  $ L.toStrict (M.requestPayload $ r)
     in
        case pObject of
            Just l@(Lo.Login name email) -> do
                    loginLookup <- A.query acid (M.QueryLogin name)
                    case loginLookup of
                        Nothing -> do
                                        A.update acid (M.InsertLogin name r l)
                        Just l2@(Lo.Login name email) ->
                            return ()
            Nothing ->
              do              
                infoM serverModuleName ("Nothing pobject " ++ show (payload)) 
                return()


sendNextSequence acid request =
      do
        emailId <- return $ M.getRequestEmail request
        lookup <- A.query acid (M.GetDatabase emailId)
        case lookup of
            Nothing ->
                do                    
                    res <- return $ M.createNextSequenceResponse emailId (Just request)
                                 $ (M.requestID request + 1)
                    infoM M.modelModuleName $ "Could not find database " ++ (show res)
                    return $ Just res
            Just x ->
                do
                    res <- return $ M.createNextSequenceResponse emailId ( Just request)
                                $ (M.nextRequestID  x request)
                    infoM M.modelModuleName $ "Using request " ++ show request
                    infoM M.modelModuleName $ ("Send next sequence " ++ (show res))
                    return $ Just res


queryNextSequence acid request =
        do
            emailId <- return $ M.getRequestEmail request
            debugM M.modelModuleName $ "Querying for " ++ emailId
            lookup <- A.query acid (M.GetDatabase emailId)
            debugM M.modelModuleName $ "Lookup " ++ (show lookup)
            case lookup of
                Nothing -> return Nothing
                Just l -> return $ Just $ M.createNextSequenceResponse
                                emailId Nothing
                                 (M.nextRequestID l request)

updateCategory acid emailId payload =
    do
        pObject <- return $ pJSON payload
        case pObject of
            Just c@(Co.Category aCat) -> do
               -- infoM "ErpModel" "Processing update category"
                lookup <- A.query acid (M.QueryCategory emailId c)
                if lookup == False then
                    do
                      A.update acid (M.InsertCategory emailId c)
                      return $ ErEr.createSuccess $ "Category added: " ++ aCat
                else
                    return $ ErEr.createSuccess $ "Category exists"
            Nothing -> return $ ErEr.createErrorS "ErpServer" "ES005" "Update category failed"


queryDatabase acid emailId payload = do
    debugM  M.modelModuleName $ "Querying database " ++ emailId
    lookup <- A.query acid (M.GetDatabase emailId)
    debugM M.modelModuleName $ "Query returned " ++ (show lookup)
    return lookup

queryParty :: A.AcidState (A.EventState M.QueryParty) 
  -> [Char] -> String -> Co.GeoLocation -> t 
  -> IO (IO (A.EventResult M.QueryParty))
queryParty acid emailId name aLocation payload = do
  debugM M.modelModuleName $ "Querying party" ++ emailId
  return $ A.query acid(M.QueryParty emailId name aLocation)

insertParty acid emailId payload =
  do
    pObject <- return $ pJSON payload
    case pObject of
        Just p -> do
          A.update acid (M.InsertParty emailId p)
          return $ ErEr.createSuccess $ "Party added " ++ (show p)
        Nothing -> return $ ErEr.createErrorS "ErpServer" "ES006" "Insert party failed"

insertCompany acid emailId payload =
    do
      debugM serverModuleName $ "Processing " ++ (show payload)
      pObject <- return $ pJSON payload
      case pObject of 
        Just p -> do
          A.update acid (M.InsertCompany emailId p)
          return $ ErEr.createSuccess $ "Company added" ++ (show p)
        Nothing -> return $ ErEr.createErrorS "ErpServer" "ES007" "Insert company failed" 

instance Exception InvalidCategory
instance Exception InvalidLogin
instance Exception InvalidRequest
instance J.ToJSON InvalidLogin
instance J.FromJSON InvalidLogin
