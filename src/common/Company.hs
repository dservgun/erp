module Company where
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
import qualified FiscalYear as FY



data Currency = Currency String
data Header = Header String
data Footer = Footer String
data Company = Company {party :: Party,
						currency :: Currency,
						alternateCurrencies :: [Currency]}
data CompanyReport = CompanyReport {fiscalYear :: FY.FiscalYear,
									company :: Company,
									header :: Header,
									footer :: Footer,
									publishDate :: UTCTime}
						

data Party = Party {name :: String,
					address :: String,
					poc		:: Contact,
					alternatePocs  :: [Contact]}
data ContactType = Phone | Mobile | Fax | Email | Website | 
					Skype |
					SIP |
					IRC |
					Jabber
					
					
data Contact = Contact {contactType :: ContactType, 
						value :: String}
				