module ErpModel where
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

import FiscalYear
type Email = String
type Name = String

data Login = Login{email :: Email, verified :: Bool} 
	deriving (Show, Generic, Typeable)
instance J.ToJSON Login
instance J.FromJSON Login


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
				
data AddressBook = AddressBook ! (M.Map Email Login)
     deriving (Typeable)


$(deriveSafeCopy 0 'base ''AddressBook)
$(deriveSafeCopy 0 'base ''Login)

  
insertEmail :: Email -> Login -> A.Update AddressBook ()  
insertEmail email aLogin
  = do AddressBook a <- get
       put (AddressBook (M.insert email aLogin a))

lookupEmail :: Email -> A.Query AddressBook (Maybe Login)
lookupEmail email =
  do AddressBook a <- ask
     return (M.lookup email a)

viewMessages :: Int  -> A.Query AddressBook [Email]
viewMessages aNumber =
  do AddressBook a <- ask
     return $ take aNumber (M.keys a)
     
$(A.makeAcidic ''AddressBook ['insertEmail, 'lookupEmail, 'viewMessages])

upsertEmail acid loginString = 
	let
		loginObject = J.decode((E.encodeUtf8 (L.fromStrict loginString)))
	in
	case loginObject of
		-- Only update verified users
		Just l@(Login anEmail True) -> A.update acid $ InsertEmail anEmail l
		Just l@(Login anEmail False) -> return ()
		_ -> return ()
		
	
getHistory acid limit = A.query acid (ViewMessages limit)

initializeDatabase  dbLocation = A.openLocalStateFrom dbLocation $ AddressBook M.empty
disconnect = A.closeAcidState

