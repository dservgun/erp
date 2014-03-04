module Company where
import Control.Monad.State
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
import FiscalYear as Fy
import qualified Currency as Cu
import qualified Login as Lo
import qualified Product as Pr
import qualified Data.Map as M
import qualified Data.Set as S



data Category = Category {category :: String, login :: Lo.Login}
    deriving(Show, Typeable, Generic, Eq, Ord)
data Header = Header String
     deriving(Show, Typeable, Generic)
data Footer = Footer String
    deriving(Show, Typeable, Generic)

data Company = Company {party :: Party,
                        currency :: Cu.Currency,
                        alternateCurrencies :: [Cu.Currency],
                        productSet :: S.Set Pr.Product}
                        deriving (Show, Typeable,Generic, Eq, Ord)
data CompanyReport = CompanyReport {fiscalYear :: Fy.FiscalYear,
                                    company :: Company,
                                    header :: Header,
                                    footer :: Footer,
                                    publishDate :: UTCTime}
                        deriving (Show, Typeable,Generic)
                        
type URI = String
data Latitude = Latitude {xpos :: Float} 
    deriving (Show, Typeable, Generic, Eq, Ord)
data Longitude = Longitude {ypos :: Float}
    deriving (Show, Typeable, Generic, Eq, Ord)
data Coordinate = Coordinate { x :: Latitude, y :: Longitude} 
    deriving (Show, Typeable, Generic, Eq, Ord)
    
data GeoLocation = GeoLocation{ uri :: URI, 
                                position :: Coordinate}
                        deriving (Show, Typeable, Generic, Eq, Ord)
                        
type VCard = String                        
data Party = Party {name :: String,
                    address :: String,
                    maplocation :: GeoLocation,
                    poc        :: Contact,
                    primaryCategory :: Category,
                    vcard :: VCard, 
                    alternateCategories :: [Category],
                    alternatePocs :: [Contact]
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
data Employee = Employee {employeeDetails :: Party, employeeCompany :: Company}                    
                    deriving (Show, Typeable, Generic, Eq, Ord)
data User = User {mainCompany :: Company, 
                  userCompany :: Company,
                  userEmployee :: Employee}
                  deriving (Show, Typeable, Generic, Eq, Ord)
type HoursPerDay = Int
type HoursPerWeek = Int
type HoursPerMonth = Int
type HoursPerYear = Int                  
data CompanyWorkTime = CompanyWorkTime {
            workTime:: Company,
            hoursPerDay :: HoursPerDay,
            hoursPerWeek :: HoursPerWeek,
            hoursPerMonth :: HoursPerMonth,
            hoursPerYear :: HoursPerYear}
                deriving (Show, Typeable, Generic, Eq, Ord)
instance J.ToJSON GeoLocation
instance J.FromJSON GeoLocation
$(deriveSafeCopy 0 'base ''GeoLocation)
instance J.ToJSON Latitude
instance J.FromJSON Latitude
instance J.FromJSON Longitude
instance J.ToJSON Longitude
$(deriveSafeCopy 0 'base ''Latitude)
$(deriveSafeCopy 0 'base ''Longitude)

instance J.ToJSON Coordinate
instance J.FromJSON Coordinate                
$(deriveSafeCopy 0 'base ''Coordinate)

instance J.ToJSON CompanyWorkTime
instance J.FromJSON CompanyWorkTime            
$(deriveSafeCopy 0 'base ''CompanyWorkTime)
instance J.ToJSON User
instance J.FromJSON User                  
instance J.ToJSON Employee
instance J.FromJSON Employee                        
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

                