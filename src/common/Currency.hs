module Currency where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import Network.URL
import qualified Country as Cou

data Currency = Currency String
    deriving (Show, Typeable, Data, Generic, Eq, Ord)

$(deriveSafeCopy 0 'base ''Currency)

instance J.ToJSON Currency
instance J.FromJSON Currency    
