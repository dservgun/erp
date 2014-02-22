module ErpModel where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Acid
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map


type Email = String
type Name = String


data AddressBook = AddressBook ! (Map.Map Email Name)
     deriving (Typeable)

$(deriveSafeCopy 0 'base ''AddressBook)

  
insertEmail :: Email -> Name -> Update AddressBook ()  
insertEmail email aName
  = do AddressBook a <- get
       put (AddressBook (Map.insert email aName a))

lookupEmail :: Email -> Query AddressBook (Maybe Name)
lookupEmail email =
  do AddressBook a <- ask
     return (Map.lookup email a)

viewMessages :: Int  -> Query AddressBook [Email]
viewMessages aNumber =
  do AddressBook a <- ask
     return $ take aNumber (Map.keys a)
     
$(makeAcidic ''AddressBook ['insertEmail, 'lookupEmail, 'viewMessages])

upsertEmail acid email aName = update acid $InsertEmail email aName
getHistory acid limit = query acid (ViewMessages limit)

initializeDatabase  dbLocation = openLocalStateFrom dbLocation $ AddressBook Map.empty
disconnect = closeAcidState