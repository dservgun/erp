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
    ErpError(..),
    ModuleError(..)
) where
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import GHC.Generics
import qualified Data.Aeson as J
import Data.Text as T


data ErpError a b = Error a | Success b deriving (Show, Eq, Ord, Typeable,Generic,Data)

type ModuleName = Text
type ErrorCode = Text
type ErrorMessage = Text
data ModuleError = ModuleError {mName :: ModuleName,
                                errorCode :: ErrorCode,
                                errorMessage :: ErrorMessage}
                                deriving (Show, Eq, Ord, Typeable, Generic, Data)

createError :: ModuleName -> ErrorCode -> ErrorMessage -> ModuleError
createError = ModuleError









