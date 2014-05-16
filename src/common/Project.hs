module Project where
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
import Data.Data
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co
import qualified Product as Pr
import qualified Stock as St
import qualified Account as Ac

type Amount = Ac.Amount
{--
XXX: Revisit
--}
data ProjectState = Opened | Done
    deriving(Show, Generic, Typeable, Eq, Ord, Data)
type Effort = Float
type Percentage = Float

data Project = Project {
    projectState :: ProjectState,
    party :: Co.Party,
    parent :: Project,
    prDependsOn :: Project,
    comment :: String}
    deriving (Show, Generic, Typeable, Eq, Ord, Data)

data Task = Task {
    parentProject :: Project,
    taskDependsOn :: Task,
    effort :: Effort}
    deriving (Show, Generic, Typeable, Eq, Ord, Data)


data Allocation = Allocation {
        employee :: Co.Employee,
        task :: Task,
        percentage :: Percentage}
        deriving (Show, Generic, Typeable, Eq, Ord, Data)


totalEffort :: Project -> Effort
-- Effort is the sum of all tasks for the project
totalEffort aProject = 0

taskLeveling :: Task -> Task
taskLeveling = id
instance J.ToJSON Project
instance J.FromJSON Project
instance J.ToJSON ProjectState
instance J.FromJSON ProjectState
instance J.ToJSON Allocation
instance J.FromJSON Allocation
instance J.ToJSON Task
instance J.FromJSON Task
$(deriveSafeCopy 0 'base ''Project)
$(deriveSafeCopy 0 'base ''ProjectState)
$(deriveSafeCopy 0 'base ''Allocation)
$(deriveSafeCopy 0 'base ''Task)




