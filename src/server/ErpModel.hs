module ErpModel where
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import Data.Dynamic
import Data.Time.Clock
import GHC.Generics
import qualified Login as Lo
import qualified Company as Co
import qualified Product as Pr
data LoginExists = LoginExists deriving (Show, Generic, Typeable, Eq, Ord)
data LoginStaleException = LoginStaleException deriving (Show, Generic, Typeable, Eq, Ord)
instance Exception LoginExists
instance Exception LoginStaleException
data ErpModel = ErpModel
                {
                    partySet :: S.Set Co.Party ,
                    companySet :: S.Set Co.Company,
                    categorySet :: S.Set Co.Category
                } deriving (Show, Generic, Typeable, Eq, Ord)
                 
{-- A given email id can be tied to only a single erp model, 
 though a given model can be associated with multiple email ids--}                 
data Database = Database ! (M.Map String Lo.Login)
     deriving (Show, Generic, Typeable, Eq, Ord)

instance J.ToJSON ErpModel
instance J.FromJSON ErpModel


$(deriveSafeCopy 0 'base ''Database)   
$(deriveSafeCopy 0 'base ''ErpModel)

emptyModel = ErpModel{partySet = S.empty,
              companySet = S.empty,
              categorySet = S.empty
              }

insertLogin :: String -> Lo.Login -> A.Update Database ()
insertLogin aString aLogin = 
    do
        Database db <- get
        put (Database (M.insert aString aLogin db))

        
lookupLogin :: String -> A.Query Database (Maybe Lo.Login)
lookupLogin aLogin =
    do
        Database db <- ask
        return (M.lookup aLogin db)
        
  
$(A.makeAcidic ''Database ['lookupLogin, 'insertLogin])

data RequestType = Create | Modify | Retrieve | Delete deriving (Show, Generic, Typeable, Eq, Ord)
type RequestEntity = String
data Request = Request {
                        requestEntity :: RequestEntity,
                        payload :: L.Text} deriving(Show, Generic, Typeable, Eq, Ord)
data InvalidRequest = InvalidRequest deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidLogin = InvalidLogin deriving (Show, Generic, Typeable, Eq, Ord)
data InvalidCategory = InvalidCategory deriving (Show, Generic, Typeable, Eq, Ord)
instance Exception InvalidCategory
instance Exception InvalidLogin
instance Exception InvalidRequest
instance J.ToJSON InvalidLogin
instance J.FromJSON InvalidLogin
instance J.ToJSON RequestType
instance J.FromJSON RequestType
instance J.ToJSON Request
instance J.FromJSON Request


initializeDatabase  dbLocation = A.openLocalStateFrom dbLocation $ Database M.empty
disconnect = A.closeAcidState

pJSON = J.decode . E.encodeUtf8 . L.fromStrict
updateDatabase acid aMessage = 
    let 
        r = J.decode $ E.encodeUtf8 $ L.fromStrict aMessage
    in 
        case r of         
        Just aRequest -> processRequest acid aRequest
        _ -> throw InvalidRequest

processRequest acid r@(Request entity payload)  = 
    case entity of
    "Login" -> updateLogin acid $ L.toStrict payload
    "Category" -> updateCategory acid $ L.toStrict payload
    _ -> throw InvalidRequest


updateLogin acid payload = 
     let
        pObject = J.decode $ E.encodeUtf8 $ L.fromStrict payload
     in 
        case pObject of
            Just l@(Lo.Login name email) -> do
                    loginLookup <- A.query acid (LookupLogin name)
                    case loginLookup of
                        Nothing -> A.update acid (InsertLogin name l)
                        Just l2@(Lo.Login name email) ->do
                            TIO.putStrLn ("Exception?")
            Nothing -> throw InvalidLogin 


updateCategory acid payload =
    let 
        pObject = pJSON payload
    in
        case pObject of
            Just (Co.Category aCat aLogin) -> do 
                return ()
            Nothing -> throw InvalidCategory