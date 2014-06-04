module ErpModel where
import System.Log.Logger
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets.Connection as WS
import Data.Dynamic
import Data.Time.Clock
import GHC.Generics
import qualified Login as Lo
import qualified SystemSequence as SSeq
import qualified Company as Co
import qualified Product as Pr
import qualified Production as Prod
import qualified Project as Proj
import qualified Stock as St
import qualified Shipment as Sh
import qualified Purchase as Pu
import qualified Sale as Sa
import qualified Forecast as Fo
import qualified Timesheet as Ts
data LoginExists = LoginExists deriving (Show, Generic, Typeable, Eq, Ord)
data LoginStaleException = LoginStaleException deriving (Show, Generic, Typeable, Eq, Ord)
data CategoryExists = CategoryExists deriving (Show, Generic, Typeable, Eq, Ord)
data LoginNotFound = LoginNotFound deriving (Show, Generic, Typeable, Eq, Ord)



type Deleted = Bool

{-Note about the versioning scheme:.  The versioning scheme is to support 
a form of an optimistic lock. Server returns the next valid request id to a client. 
Any request that comes with a value less than the last request id is considered to be
stale and the stale request is  returned to the client as part of an error. -}


data ErpModel = ErpModel
                {
                    login :: Lo.Login,
                    partySet :: S.Set Co.Party,
                    companySet :: S.Set Co.Company,
                    categorySet :: S.Set Co.Category,
                    deleted :: Deleted,
                    requestSet :: S.Set Request,
                    responseSet :: S.Set Response,
                    lastRequestID :: ID,
                    lastResponseID :: ID
                } deriving (Show, Generic, Typeable, Eq, Ord)


delete anErpModel = anErpModel {deleted = True}
{-- A given email id can be tied to only a single erp model,
 though a given model can be associated with multiple email ids--}
data Database = Database ! (M.Map String ErpModel)
     deriving (Show, Generic, Typeable, Eq, Ord)
data RequestType = Create | Modify | Retrieve | Delete deriving (Show, Generic, Typeable, Eq, Ord)
type RequestEntity = String
type ProtocolVersion = String
type ID = Integer

data ErrorResponse = ErrorResponse {
    errorResponseID :: ID,
    errorResponseVersion :: ProtocolVersion,
    errorIncomingRequest :: Request,
    errorMessage :: L.Text 
} deriving (Show, Generic, Typeable, Eq, Ord)

data Response = Response {
    responseID :: ID,
    responseVersion :: ProtocolVersion,
    incomingRequest :: Maybe Request,
    responsePayload :: L.Text } deriving (Show, Generic, Typeable, Eq, Ord)

            
createNextSequenceResponse emailId c anID = Response anID  protocolVersion c 
            $ L.pack $ show anID
data Request = Request {
    requestID :: ID,
    requestVersion :: ProtocolVersion,
    requestEntity :: RequestEntity,
    emailId :: String,
    requestPayload :: L.Text} deriving(Show, Generic, Typeable, Eq, Ord)
getRequestEmail aRequest = emailId aRequest

data InvalidRequest = InvalidRequest deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidLogin = InvalidLogin deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidCategory = InvalidCategory deriving (Show, Generic, Typeable, Eq, Ord)

-- The current protocol build version.
-- This needs to be validated before processing
-- a request.We should get the build version,
-- from the build instead of using a string as below.
-- Version naming protocol should be similar to
-- what most systems do today, so basic
-- increments will still compare.
-- We need to maintain some amount
-- of backward compatibility, though,
-- that is probably debatable?
protocolVersion :: ProtocolVersion
protocolVersion = "0.0.0.1"
-- ID is a string read and written from an integer

-- Simple integer should suffice.
nextID :: ID -> ID
nextID  x =  x + 1

-- Any message with this id is an error id.
errorID :: ID
errorID = -1



emptyModel = ErpModel {
                partySet = S.empty,
                categorySet = S.empty,
                companySet = S.empty,
                login = Lo.empty,
                requestSet = S.empty,
                responseSet = S.empty,
                deleted = False,
                lastRequestID = 0,
                lastResponseID = 0
              }


uLastRequestID :: ErpModel -> ErpModel 
uLastRequestID aModel = aModel {lastRequestID = nextID 
            $ lastRequestID aModel
        }
loginEmail :: ErpModel -> Lo.Email 
loginEmail anErpModel = Lo.getLoginEmail $ login anErpModel

insertRequest ::  ErpModel -> Request -> ErpModel
insertRequest aModel aRequest = aModel {
                requestSet = S.insert aRequest (requestSet aModel)
            ,   lastRequestID = requestID aRequest
            }
insertResponse :: ErpModel -> Response -> ErpModel
insertResponse aModel aResponse = aModel {responseSet =
            S.insert aResponse (responseSet aModel)
            , lastResponseID = responseID aResponse
        }


supportedVersions :: ErpModel -> S.Set ProtocolVersion
supportedVersions aModel = 
    let 
        reqVersions = S.map  (\x -> requestVersion x)   ( requestSet aModel)
        resVersions  = S.map   (\x -> responseVersion x)  (responseSet aModel)
    in 
        reqVersions
 

updateModel :: ErpModel -> Co.Category -> ErpModel
updateModel aModel aCategory = aModel{ categorySet = S.insert aCategory (categorySet aModel)}

updateParty :: ErpModel -> Co.Party -> ErpModel
updateParty aModel aParty = aModel {partySet = S.insert aParty (partySet aModel)}



lookupParty :: String -> String -> Co.GeoLocation -> 
    A.Query Database (Maybe Co.Party)
lookupParty aLogin aName aLocation  =
    do
        Database db <- ask
        let erp = M.lookup aLogin db
        case erp of
            Nothing -> throw Co.CompanyNotFound
            Just x -> return $ Co.findParty (aName, aLocation) (partySet x)

insertParty :: String -> Co.Party -> A.Update Database ()
insertParty aLogin p =
    do
        Database db <- get
        let erp = M.lookup aLogin db
        case erp of
            Just exists -> put(Database (M.insert aLogin (updateParty exists p) db))
            _ -> return ()

deleteParty :: String -> Co.Party -> A.Update Database ()
deleteParty aLogin aParty = do
    Database db <- get
    let erp = M.lookup aLogin db
    case erp of
        Just found -> put (Database (M.insert aLogin (delP2 aParty found) db))
        _ -> return ()
    where
        delP2 aParty model = model {partySet = S.delete aParty (partySet model)}


lookupCompany :: String -> Co.Party -> A.Query Database (Maybe Co.Company)
lookupCompany aLogin aParty =
    do
        Database db <- ask
        let erp = M.lookup aLogin db
        case erp of
            Nothing -> throw Co.CompanyNotFound
            Just x -> return $ Co.findCompany aParty (companySet x)


insertLogin :: String -> Lo.Login -> A.Update Database ()
insertLogin aString aLogin =
    do
        Database db <- get
        let loginErp = emptyModel {login = aLogin}
        put (Database (M.insert aString loginErp db))

deleteLogin :: String -> A.Update Database ()
deleteLogin aString  =
    do
        Database db <- get
        let loginErp = M.lookup aString db
        case loginErp of
            Nothing -> return ()            
            Just x -> put (Database (M.insert aString (delete x) db))



lookupLogin :: String -> A.Query  Database (Maybe Lo.Login)
lookupLogin aLogin =
    do
        Database db <- ask
        let erp = M.lookup aLogin db
        case erp of
            Just erp -> return $ Just $ login erp
            _   -> return Nothing

lIsMember :: Co.Category -> S.Set Co.Category -> Maybe Bool
lIsMember aCategory aSet = if S.member aCategory aSet then Just True else Nothing

decodeCategory :: Maybe Co.Category -> Maybe Bool -> Maybe Co.Category
decodeCategory Nothing _ = Nothing
decodeCategory (Just x )  Nothing = Nothing
decodeCategor (Just x) (Just y) = Just x

exists :: Co.Category -> Maybe ErpModel -> Maybe Co.Category
exists aKey Nothing = Nothing
exists aKey (Just e) = decodeCategory (Just aKey ) (lIsMember aKey $ categorySet e)

lookupCategory :: String -> Co.Category -> A.Query  Database(Maybe Co.Category)
-- qbe -> query by example
lookupCategory aLogin qbe =
    do
       Database db <- ask
       let erp = M.lookup aLogin db
       return $ exists qbe erp

insertCategory :: String -> Co.Category -> A.Update Database ()
insertCategory aLogin c@(Co.Category aCatName) =
    do
        Database db <- get
        let erp = M.lookup aLogin db
        case erp of
            Just exists -> put(Database (M.insert aLogin (updateModel exists c) db))
            _       -> return()


getDatabase :: String -> A.Query Database (Maybe ErpModel)
getDatabase userEmail = do
        Database db <- ask
        let loginErp = M.lookup userEmail db
        return loginErp

updateLastRequestID :: String -> A.Update Database ()
updateLastRequestID userEmail = do
    Database db <- get
    let loginErp = M.lookup userEmail db
    case loginErp of
        Nothing -> return()
        Just x -> put (Database $M.insert userEmail (uLastRequestID x) db)

   
$(A.makeAcidic ''Database [
    'lookupLogin, 'insertLogin,
    'deleteLogin, 'lookupCategory, 'insertCategory
            , 'getDatabase
            , 'updateLastRequestID ])


initializeDatabase  dbLocation = A.openLocalStateFrom dbLocation $ Database M.empty
disconnect = A.closeAcidState


updateDatabase connection acid aMessage =
    let
        r = J.decode $ E.encodeUtf8 $ L.fromStrict aMessage
    in
        case r of
        Just aRequest -> processRequest connection acid aRequest
        _ -> throw InvalidRequest

sendTextData connection aText = WS.sendTextData connection aText
sendError connection request aMessage = 
    let 
        response = ErrorResponse errorID protocolVersion request aMessage
    in
        WS.sendTextData connection $ J.encode response

queryNextSequenceConstant = "QueryNextSequence"
addLoginConstant = "Login"
deleteLoginConstant = "DeleteLogin"
updateCategoryConstant = "UpdateCategory"
queryDatabaseConstant = "QueryDatabase"
closeConnectionConstant= "CloseConnection"

processRequest connection acid r@(Request iRequestID 
        iProtocolVersion entity emailId payload)  =
    if iProtocolVersion /= protocolVersion then
        do
        debugM ErpModel.moduleName $  "Invalid protocol message " ++ iProtocolVersion
        sendError connection r $ L.pack ("Invalid protocol version : " ++ protocolVersion)
    else 
        do

            debugM ErpModel.moduleName $ "Incoming request " ++ (show r)
            case entity of
                "QueryNextSequence"-> do
                    debugM ErpModel.moduleName $ "Processing message  " ++ (show entity)
                    response <- queryNextSequence acid emailId
                    debugM ErpModel.moduleName $  "processing " ++ (show response)
                    case response of 
                        Nothing -> sendError connection r "QueryNextSequence failed"
                        Just x -> sendTextData connection $ J.encode x
                "Login" -> do
                        updateLogin acid $ L.toStrict payload
                        nextSequenceResponse <- sendNextSequence acid  $ getEmail (L.toStrict payload)
                        case nextSequenceResponse of 
                            Just x -> WS.sendTextData connection  $ J.encode x
                            Nothing -> sendError connection r "Add login request failed"
                "DeleteLogin" -> deleteLoginA acid emailId
                "UpdateCategory" -> updateCategory acid emailId $ L.toStrict payload
                "QueryDatabase"  -> do
                        model <- queryDatabase acid emailId $ L.toStrict payload
                        TIO.putStrLn $ T.pack $ show model
                "CloseConnection" -> do
                            debugM ErpModel.moduleName $  "Closing connection for "  ++ emailId                            
                            WS.sendTextData connection $ J.encode r
                _ -> throw InvalidRequest

deleteLoginA acid anEmailId = A.update acid (DeleteLogin anEmailId)

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
updateLogin acid payload =
     let
        pObject = pJSON payload
     in
        case pObject of
            Just l@(Lo.Login name email) -> do
                    loginLookup <- A.query acid (LookupLogin name)
                    case loginLookup of
                        Nothing -> do 
                                        A.update acid (InsertLogin name l)
                                        return $ Just name
                        Just l2@(Lo.Login name email) ->return Nothing
            Nothing -> throw InvalidLogin

sendNextSequence acid emailId = do
    debugM ErpModel.moduleName $ "Sending next sequence number " ++ emailId
    lookup <- A.query acid (GetDatabase emailId)
    case lookup of 
        Nothing -> 
            do
                let 
                    res =createNextSequenceResponse emailId Nothing $ errorID
                debugM ErpModel.moduleName $ "Could not find database " ++ (show res)
                return $ Just res 
        Just x -> 
            do
                let 
                    res = createNextSequenceResponse emailId Nothing $ lastRequestID x
                debugM ErpModel.moduleName $ "Database found " ++ (show res)
                debugM ErpModel.moduleName $ show res
                return $ Just res

queryNextSequence acid emailId = 
    do
        debugM ErpModel.moduleName $ "Querying for " ++ emailId
        A.update acid (UpdateLastRequestID emailId)
        lookup <- A.query acid (GetDatabase emailId)
        debugM ErpModel.moduleName $ "Lookup " ++ (show lookup)
        case lookup of
            Nothing -> return Nothing
            Just l -> return $ Just $ createNextSequenceResponse 
                            emailId Nothing
                            $ lastRequestID l

updateCategory acid emailId payload =
    let
        pObject = pJSON payload
    in
        case pObject of
            Just c@(Co.Category aCat) -> do
               -- infoM "ErpModel" "Processing update category"
                lookup <- A.query acid (LookupCategory emailId c)
                case lookup of
                    Nothing -> A.update acid (InsertCategory emailId c)
                    Just c@(Co.Category aCat) -> return ()
            Nothing -> return ()

moduleName :: String
moduleName = "ErpModel"

queryDatabase acid emailId payload = do
    debugM  ErpModel.moduleName $ "Querying database " ++ emailId
    lookup <- A.query acid (GetDatabase emailId)
    debugM ErpModel.moduleName $ "Query returned " ++ (show lookup)
    return lookup

displayText = T.pack . show
pJSON = J.decode . E.encodeUtf8 . L.fromStrict


$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''ErpModel)
$(deriveSafeCopy 0 'base ''Request)
$(deriveSafeCopy 0 'base ''Response)



instance Exception InvalidCategory
instance Exception InvalidLogin
instance Exception InvalidRequest
instance J.ToJSON InvalidLogin
instance J.FromJSON InvalidLogin
instance J.ToJSON RequestType
instance J.FromJSON RequestType
instance J.ToJSON Request
instance J.FromJSON Request
instance J.ToJSON ErpModel
instance J.FromJSON ErpModel
instance J.ToJSON Response
instance J.FromJSON Response
instance J.ToJSON ErrorResponse
instance J.FromJSON ErrorResponse
