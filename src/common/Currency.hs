module Currency where
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
import Network.URL

type ISO_CODE_ALPHA_2 = String

data Country = Country { iso_code :: ISO_CODE_ALPHA_2, 
        name :: String,
        url :: String,
        description :: String}    
    deriving (Show, Typeable, Generic, Eq, Ord)
data Currency = Currency String
    deriving (Show, Typeable, Generic, Eq, Ord)

getURL :: Country -> Maybe URL
getURL (Country _ _ url _) = importURL url

updateURL :: Country -> URL -> Country 
updateURL aCountry aURL = aCountry {url = exportURL aURL}
$(deriveSafeCopy 0 'base ''Currency)
$(deriveSafeCopy 0 'base ''Country)
instance J.ToJSON Currency
instance J.FromJSON Currency    
instance J.ToJSON Country
instance J.FromJSON Country
