module ErpModel where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Acid
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson
import Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import FiscalYear

type Email = String
type Name = String

data Login = Login{email :: Email, verified :: Bool} 
	deriving (Show, Generic, Typeable)
instance ToJSON Login
instance FromJSON Login


data RequestType = Create | Modify | Retrieve | Delete
data EntityState = Open | Closed

-- Note about RequestType: 
-- The initial model of Request = {r : RequestType, e : GenericDbEntity} 
-- results in recursive types that the compiler doesnt like.
data Currency = Currency String
data Header = Header String
data Footer = Footer String
data Company = Company {party :: Party,
						currency :: Currency,
						alternateCurrencies :: [Currency]}
data CompanyReport = CompanyReport {fiscalYear :: FiscalYear,
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
				
data AddressBook = AddressBook ! (Map.Map Email Login)
     deriving (Typeable)


$(deriveSafeCopy 0 'base ''AddressBook)
$(deriveSafeCopy 0 'base ''Login)

  
insertEmail :: Email -> Login -> Update AddressBook ()  
insertEmail email aLogin
  = do AddressBook a <- get
       put (AddressBook (Map.insert email aLogin a))

lookupEmail :: Email -> Query AddressBook (Maybe Login)
lookupEmail email =
  do AddressBook a <- ask
     return (Map.lookup email a)

viewMessages :: Int  -> Query AddressBook [Email]
viewMessages aNumber =
  do AddressBook a <- ask
     return $ take aNumber (Map.keys a)
     
$(makeAcidic ''AddressBook ['insertEmail, 'lookupEmail, 'viewMessages])

upsertEmail acid loginString = 
	let
		loginObject = decode((E.encodeUtf8 (L.fromStrict loginString)))
	in
	case loginObject of
		-- Only update verified users
		Just l@(Login anEmail True) -> update acid $ InsertEmail anEmail l
		Just l@(Login anEmail False) -> return ()
		_ -> return ()
		
	
getHistory acid limit = query acid (ViewMessages limit)

initializeDatabase  dbLocation = openLocalStateFrom dbLocation $ AddressBook Map.empty
disconnect = closeAcidState

