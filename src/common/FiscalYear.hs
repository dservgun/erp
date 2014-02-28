module FiscalYear where
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
import qualified Entity  as Es

data PeriodType = Standard | Adjustment
    deriving(Show, Enum, Bounded, Typeable, Generic, Eq, Ord)

data FiscalYear = FiscalYear {fyName :: String, 
                          fyCode :: String,
                          startDate :: UTCTime,
                          endDate :: UTCTime,
                          state :: Es.EntityState}
                          deriving (Show, Typeable, Generic, Eq, Ord)

data FiscalYearPeriod = FiscalYearPeriod 
                {period :: String,
                 year :: FiscalYear,
                 fyPeriodType :: PeriodType}
                    deriving(Show, Typeable, Generic, Eq, Ord)

                    
instance J.ToJSON FiscalYear
instance J.FromJSON FiscalYear
instance J.ToJSON FiscalYearPeriod
instance J.FromJSON FiscalYearPeriod
instance J.ToJSON PeriodType
instance J.FromJSON PeriodType


$(deriveSafeCopy 0 'base ''FiscalYear)
$(deriveSafeCopy 0 'base ''FiscalYearPeriod)
$(deriveSafeCopy 0 'base ''PeriodType)                    

getFYPeriodTypes = map(\x -> (L.pack (show x),x)) ([minBound..maxBound]::[PeriodType])