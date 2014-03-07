module Stock where
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

data LocationType = Storage | Warehouse | Customer | Supplier | LostAndFound
    deriving(Show, Eq, Ord, Typeable, Generic)

data Location = Location{ 
                location :: Co.Address,
                locationType :: LocationType
                } deriving (Show, Eq, Ord, Typeable, Generic)
data MoveState = Draft | Assigned | Done | Cancel
    deriving (Show, Eq, Ord, Typeable, Generic)
data Move = Move {
            product:: Pr.Product,
            sourceLocation :: Location,
            destination :: Location,
            moveState :: MoveState
        } deriving (Show, Eq, Ord, Typeable, Generic)
        
$(deriveSafeCopy 0 'base ''Move)
$(deriveSafeCopy 0 'base ''MoveState)
$(deriveSafeCopy 0 'base ''Location)
$(deriveSafeCopy 0 'base ''LocationType)
instance J.ToJSON Move
instance J.FromJSON Move
instance J.ToJSON MoveState
instance J.FromJSON MoveState
instance J.ToJSON Location
instance J.FromJSON Location
instance J.ToJSON LocationType
instance J.FromJSON LocationType        