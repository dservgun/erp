-----------------------------------------------------------------------------
--
-- Module      :  ErpError
-- Copyright   :
-- License     :  GPL Nothing
--
-- Maintainer  :  dinkar.ganti@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------



module ErpError (
    ErpError(Error,Success),
    createError,
    createSuccess,
    createErrorS,
    getString,
    ModuleError
) where
import Data.Acid.Remote()
import Data.SafeCopy
import Data.Typeable
import Data.Data
import GHC.Generics
import qualified Data.Aeson as J
import Data.Text.Lazy as L


data ErpError a b = Error a | Success b deriving (Show, Eq, Ord, Typeable,Generic,Data)

-- newtype ErpErrorT m a b = ErpErrorT {
--     runMaybeT :: m (ErpError a b)
-- }

type ModuleName = L.Text
type ErrorCode = L.Text
type ErrorMessage = L.Text
data ModuleError = ModuleError {
      mName :: ModuleName,
      errorCode :: ErrorCode,
      errorMessage :: ErrorMessage
    } deriving (Show, Eq, Ord, Typeable, Generic, Data)

createError :: ModuleName -> ErrorCode -> ErrorMessage -> ModuleError
createError = ModuleError

createErrorS :: String -> String -> String -> ErpError ModuleError a
createErrorS a b c = Error $ createError (L.pack a) (L.pack b) (L.pack c)

createSuccess :: a -> ErpError ModuleError a
createSuccess a = Success a

-- hack to work around the typesystem.

getString :: ErpError ModuleError a0 -> L.Text
getString e@(Error modError) = L.pack $ (unpack mName) ++ (unpack errorCode) ++ (unpack errorMessage)
    where unpack x = L.unpack $ x modError

$(deriveSafeCopy 0 'base ''ErpError)
$(deriveSafeCopy 0 'base ''ModuleError)



