module ErpModel where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Login as Lo
import qualified Company as Co

data ErpModel = ErpModel
                {company :: Co.Company,
                 login :: Lo.Login} deriving(Show, Generic, Typeable, Eq, Ord)
{-- A given email id can be tied to only a single erp model, 
 though a given model can be associated with multiple email ids--}                 
data Database = Database ! (M.Map Lo.Login ErpModel)
     deriving (Show, Generic, Typeable, Eq, Ord)

instance J.ToJSON ErpModel
instance J.FromJSON ErpModel


$(deriveSafeCopy 0 'base ''Database)   
$(deriveSafeCopy 0 'base ''ErpModel)

  
$(A.makeAcidic ''Database [])

data RequestType = Create | Modify | Retrieve | Delete

initializeDatabase  dbLocation = A.openLocalStateFrom dbLocation $ Database M.empty
disconnect = A.closeAcidState

updateDatabase acid aMessage = 
    let 
        object = J.decode $ E.encodeUtf8 $ L.fromStrict aMessage
    in 
        case object of
            Just l@(Lo.Login email verified) -> updateLogin acid l
            Nothing -> return() 
    

updateLogin acid aLogin = return ()
updateCategory acid aCategory = return ()
updateCompany acid aCompany = return ()    
