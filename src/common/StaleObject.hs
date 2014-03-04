module StaleObject where
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy as Ds
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics

type VersionNumber = Int
data StaleObjectException = StaleObjectException deriving(Show, Eq, Generic, Typeable, Ord)
instance Exception StaleObjectException

data VersionI = Version{ verNum :: Int}
class Version a where
    update :: a -> a -> a
    version :: a -> VersionNumber

instance StaleObject.Version VersionI where
    update v@(StaleObject.Version vn) v2@(StaleObject.Version vo) = 
        if vn < vo then
            throw StaleObjectException
        else
            v
    version (StaleObject.Version aNum) = aNum            
    
        