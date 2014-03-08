module Forecast where
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

data ForecastState = Draft | Done | Cancel
            deriving(Eq, Ord, Enum, Bounded, Typeable, Generic, Show)
data ForecastLine = ForecastLine {
                    flProduct :: Pr.Product,
                    quantity :: Ac.Quantity,
                    minimalQuantity :: Ac.Quantity,
                    unitOfMeasure :: Pr.UOM
            } deriving (Show, Eq, Ord, Typeable, Generic)
                   
data Forecast = Forecast {
            sfLoc:: St.Location,
            destination :: St.Location, -- Customer location
            startDate :: UTCTime,
            endDate :: UTCTime,
            sfCompany :: Co.Company,
            forecastLines :: [ForecastLine],
            forecastState :: ForecastState
            } deriving (Show, Eq, Ord, Typeable, Generic)

            
orderPoint :: St.ProductQuantities -> Forecast -> Bool
orderPoint pq aForecast =  False -- Need to make in monadic
            
instance J.ToJSON ForecastState
instance J.FromJSON ForecastState
instance J.ToJSON ForecastLine
instance J.FromJSON ForecastLine
instance J.ToJSON Forecast
instance J.FromJSON Forecast

$(deriveSafeCopy 0 'base ''ForecastState)
$(deriveSafeCopy 0 'base ''ForecastLine)
$(deriveSafeCopy 0 'base ''Forecast)