module Bank where
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
import FiscalYear as Fy
import Company as Co
import Currency as Cu

instance J.ToJSON BranchCode
instance J.FromJSON BranchCode
instance J.ToJSON BIC
instance J.FromJSON BIC
instance J.ToJSON Bank
instance J.FromJSON Bank
type BankCode = String
type Location = String                
    
data BranchCode = PrimaryOffice String | Office String
                   deriving(Show, Typeable, Generic, Eq, Ord)

primaryOffice :: BranchCode                   
primaryOffice = PrimaryOffice "XXX"

data BIC =  BIC { code :: BankCode,
                  country :: Cu.Country,
                  location :: Location,
                  branchCode :: BranchCode} 
                  deriving (Show, Typeable, Generic, Eq, Ord)
data Bank = Bank{
            party :: Co.Party,
            bics :: [BIC]
            } deriving (Show, Typeable, Generic, Eq, Ord)

$(deriveSafeCopy 0 'base ''BranchCode)
$(deriveSafeCopy 0 'base ''BIC)
$(deriveSafeCopy 0 'base ''Bank)            