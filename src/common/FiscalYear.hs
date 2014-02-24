module FiscalYear where
import Data.Time.Clock
import Entity(EntityState)
data PeriodType = Standard | Adjustment

data FiscalYear = FiscalYear {name :: String, 
						  code :: String,
						  startDate :: UTCTime,
						  endDate :: UTCTime,
						  state :: EntityState}
data FiscalYearPeriod = FiscalYearPeriod 
				{period :: String,
				 year :: FiscalYear,
				 fyPeriodType :: PeriodType}
