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

data RequestType = Create | Modify | Retrieve | Delete
data EntityState = Open | Closed

initializeDatabase  dbLocation = A.openLocalStateFrom dbLocation $ Lo.AddressBook M.empty
disconnect = A.closeAcidState

