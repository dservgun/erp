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
import FiscalYear as Fy
import qualified Currency as Cu



data Category = Category String
    deriving(Show, Typeable, Generic, Eq, Ord)
data Header = Header String
     deriving(Show, Typeable, Generic)
data Footer = Footer String
    deriving(Show, Typeable, Generic)
data Company = Company {party :: Party,
                        currency :: Cu.Currency,
                        alternateCurrencies :: [Cu.Currency]}
                        deriving (Show, Typeable,Generic, Eq, Ord)

data CompanyReport = CompanyReport {fiscalYear :: Fy.FiscalYear,
                                    company :: Company,
                                    header :: Header,
                                    footer :: Footer,
                                    publishDate :: UTCTime}
                        deriving (Show, Typeable,Generic)
                        

data Party = Party {name :: String,
                    address :: String,
                    poc        :: Contact,
                    alternatePocs  :: [Contact],
                    primaryCategory :: Category,
                    alternateCategories :: [Category]
                    }
                    deriving (Show, Typeable,Generic, Eq, Ord)
data ContactType = Phone | Mobile | Fax | Email | Website | 
                    Skype |
                    SIP |
                    IRC |
                    Jabber
                    deriving (Enum, Bounded, Show, Typeable,Generic, Eq, Ord)
                    
                    
data Contact = Contact {contactType :: ContactType, 
                        value :: String}
                    deriving(Show, Typeable,Generic, Eq, Ord)
                        
instance J.ToJSON Company
instance J.FromJSON Company
instance J.ToJSON Contact
instance J.FromJSON Contact
instance J.ToJSON ContactType
instance J.FromJSON ContactType
instance J.ToJSON Party
instance J.FromJSON Party
instance J.ToJSON Category
instance J.FromJSON Category
instance J.ToJSON Header
instance J.FromJSON Header
instance J.ToJSON Footer
instance J.FromJSON Footer
instance J.ToJSON CompanyReport
instance J.FromJSON CompanyReport

$(deriveSafeCopy 0 'base ''Category)
$(deriveSafeCopy 0 'base ''Company)
$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''ContactType)
$(deriveSafeCopy 0 'base ''Party)

$(deriveSafeCopy 0 'base ''Header)
$(deriveSafeCopy 0 'base ''Footer)
$(deriveSafeCopy 0 'base ''CompanyReport)

getContactTypes = map(\x -> (L.pack (show x),x)) ([minBound..maxBound]::[ContactType])

                