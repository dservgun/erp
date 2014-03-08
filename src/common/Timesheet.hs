module Timesheet where
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
import qualified Account as Ac
import qualified Stock as St

data Work = Work {
    name :: String,
    active :: Bool,
    parent :: Work,
    availableOnTimesheets :: Bool,
    company :: Co.Company}
    deriving (Show, Eq, Ord, Typeable, Generic)

data Timesheet = Timesheet {
        work :: Work,
        employee :: Co.Employee,
        timeSpent :: Ac.TimeSpent,
        date :: UTCTime,
        description :: String}
        deriving (Show, Eq, Ord, Typeable, Generic)
$(deriveSafeCopy 0 'base ''Work)
$(deriveSafeCopy 0 'base ''Timesheet)
instance J.ToJSON Work
instance J.FromJSON Work
instance J.ToJSON Timesheet
instance J.FromJSON Timesheet
        