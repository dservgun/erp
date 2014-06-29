module ErpServer(serverMain, testServerMain, serverModuleName
    , handleConnection
    , IncomingRequestType(..))where

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

displayText = T.pack . show
pJSON = J.decode . E.encodeUtf8 . L.fromStrict


data InvalidRequest = InvalidRequest deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidLogin = InvalidLogin deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidCategory = InvalidCategory deriving (Show, Generic, Typeable, Eq, Ord)
data IncomingRequestType = QueryNextSequence | Login | DeleteLogin | UpdateCategory | QueryDatabase | CloseConnection
    deriving (Show, Read, Generic, Typeable, Eq, Ord)


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
     updateDatabase conn acid msg
     where
       catchDisconnect e =
         case fromException e of
           Just  WS.ConnectionClosed ->
                 do
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
                res  <- processRequest connection acid aRequest
                res2 <- updateRequests connection acid aRequest
                postProcessRequest connection acid aRequest
        _ -> do
              errorM serverModuleName (show r)
              M.sendError connection Nothing "Error"
              return $ ErEr.createErrorS "ErpServer" "ES002" $ "Invalid message " ++ (show aMessage)

updateRequests connection acid r = A.update acid(M.InsertRequest r)
postProcessRequest connection acid r = do
  nextSequenceResponse <- sendNextSequence acid  r
  case nextSequenceResponse of
    Just x -> do
              WS.sendTextData connection  $ J.encode x
              A.update acid (M.InsertResponse x)
    Nothing -> do
        let moduleError = ErEr.createErrorS "ErpServer" "ES001" $ "Invalid response " ++ show r
        M.sendError connection (Just r) $ L.pack $ show moduleError
        return moduleError


-- // Error RoutingError *Maybe instance?* Either*
routeRequest QueryNextSequence = return ()
routeRequest Login             = updateLogin acid r
routeRequest DeleteLogin       = deleteLoginA acid emailId
routeRequest UpdateCategory    = updateCategory acid emailId $ L.toStrict payload
routeRequest QueryDatabase     = do
  model <- queryDatabase acid emailId $ L.toStrict payload
  infoM serverModuleName (show model)
routeRequest CloseConnection   = do
        response <- return $ M.createCloseConnectionResponse r
        debugM M.modelModuleName $ "ErpModel::Sending " ++ (show response)
        WS.sendTextData connection $ J.encode response

checkProtocol :: String -> a -> Either String a
checkProtocol iProtocolVersion = -- Returns a ErpError if wrong protocol
    if iProtocolVersion /= M.protocolVersion then a
    else (Left "Protocol Not Supported") -- TODO: ErpError

processRequest :: WS.Connection -> AcidState (EventState M.GetDatabase) -> M.Request -> IO()
processRequest connection acid r@(M.Request iRequestID requestType iProtocolVersion entity emailId payload) =
    case (checkProtocol iProtocolVersion processEntity) of
     :: Either String (IO())

        where processEntity = do
                entityType <- read <$> entity
                debugM M.modelModuleName $ "Incoming request " ++ (show r)
                currentRequest <- checkRequest acid r
                if currentRequest then
                    routeRequest currentRequest
                else
                    do
                      let moduleError = ErEr.createErrorS "ErpServer" "ES002" $ "Stale message " ++ show r
                          M.sendError connection  (Just r)  $ ErEr.getString moduleError


deleteLoginA acid anEmailId = A.update acid (M.DeleteLogin anEmailId)

getEmail payload =
    let
        pObject = pJSON payload
    in
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
      infoM serverModuleName ("Querying erp returned " ++ (show erp))
      case erp of
        Nothing ->  return True
        Just x -> return $  ( M.nextRequestID x ) == iRequestID


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
                                 $ SSeq.errorID
                    debugM M.modelModuleName $ "Could not find database " ++ (show res)
                    return $ Just res
            Just x ->
                do
                    let
                        res = M.createNextSequenceResponse emailId ( Just request)
                                $ (M.nextRequestID  x)
                    return $ Just res


queryNextSequence acid request =
    let
        emailId = M.getRequestEmail request
    in
        do
            debugM M.modelModuleName $ "Querying for " ++ emailId
            lookup <- A.query acid (M.GetDatabase emailId)
            debugM M.modelModuleName $ "Lookup " ++ (show lookup)
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
                lookup <- A.query acid (M.QueryCategory emailId c)
                if lookup == False then
                    A.update acid (M.InsertCategory emailId c)
                else
                    return ()
            Nothing -> return ()


queryDatabase acid emailId payload = do
    debugM  M.modelModuleName $ "Querying database " ++ emailId
    lookup <- A.query acid (M.GetDatabase emailId)
    debugM M.modelModuleName $ "Query returned " ++ (show lookup)
    return lookup

queryParty acid emailId name aLocation payload = do
  debugM M.modelModuleName $ "Querying party" ++ emailId
  lookup <- A.query acid(M.QueryParty emailId name aLocation)
  return lookup

instance Exception InvalidCategory
instance Exception InvalidLogin
instance Exception InvalidRequest
instance J.ToJSON InvalidLogin
instance J.FromJSON InvalidLogin
instance J.ToJSON IncomingRequestType
instance J.FromJSON IncomingRequestType
