module AccountAsset where
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
import qualified Account as Ac
type AssetValue = Float
data DepreciationMethod = Linear | NonLinear deriving(Show, Typeable, Generic, Eq, Ord)
-- GeneralMonth for odd frequencies if they exist.
-- These could be used for computing asset values at different 
-- maturities if needed.
data Frequency = Monthly | Quarterly | Semi_Annual | Yearly | GeneralMonth Int
    | GeneralYear Int deriving (Show, Typeable, Generic, Eq, Ord)
data AssetState = Draft | Running | Closed deriving (Show, Typeable, Generic, Eq, Ord)    
data Asset = Asset {
    journal :: Ac.Journal,
    value ::  AssetValue,
    residualValue :: AssetValue,
    startDate :: UTCTime,
    endDate :: UTCTime,
    depreciationMethod :: DepreciationMethod,
    frequency :: Frequency,
    assetState :: AssetState} 
    deriving (Show, Typeable, Generic, Eq ,Ord)

data AssetLine = AssetLine {
        asset :: Asset,
        assetDate :: UTCTime,
        assetValue :: AssetValue}
        deriving (Show, Typeable, Generic, Eq, Ord)

$(deriveSafeCopy 0 'base ''Asset)
$(deriveSafeCopy 0 'base ''AssetLine)
$(deriveSafeCopy 0 'base ''AssetState)
$(deriveSafeCopy 0 'base ''DepreciationMethod)
$(deriveSafeCopy 0 'base ''Frequency)

        