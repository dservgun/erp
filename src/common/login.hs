{-- 
    .. Notation is important as per this link:
    exporting data constructors: http://en.wikibooks.org/wiki/Haskell/Modules.
--}
module Login(Login (..), 
            Email,
            Name,
            AddressBook(..),
            insertEmail, 
             lookupEmail,
             viewMessages, 
             upsertEmail,             
             getHistory)
    where

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

type Email = String
type Name = String

data Login = Login{email :: Email, verified :: Bool} 
	deriving (Show, Generic, Typeable)
instance J.ToJSON Login
instance J.FromJSON Login
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
