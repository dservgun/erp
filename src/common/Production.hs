module Production where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Tree as Tr
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co
import qualified Product as Pr
data Production = 
    Production{ product :: Pr.Product,
                inputs :: [Move],
                outputs :: [Move],
                productionState :: ProductionState}
    deriving(Show, Enum, Bounded, Typeable, Generic, Eq, Ord)

data ProductionState = Request | Draft | Waiting | Assigned | Running | Done
    deriving (Show, Enum, Bounded, Typeable, Generic, Eq, Ord)

$(deriveSafeCopy 0 'base ''Production)
$(deriveSafeCopy 0 'base ''ProductionState)
instance J.ToJSON Production
instance J.FromJSON Production
instance J.ToJSON ProductionState
instance J.FromJSON ProductionState
    