module Carrier where
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
import Bank as Ba

data CarrierCostMethod = PercentCostMethod Float | 
                         WeightCostMethod Float deriving (Show, Typeable, Generic, Eq, Ord)
data Carrier = Carrier {party :: Party} deriving(Show, Typeable, Generic, Eq, Ord)


instance J.ToJSON Carrier
instance J.FromJSON Carrier
instance J.ToJSON CarrierCostMethod
instance J.FromJSON CarrierCostMethod

$(deriveSafeCopy 0 'base ''Carrier)
