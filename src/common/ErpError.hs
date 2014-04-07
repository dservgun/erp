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
    ErpError(..)
) where
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import GHC.Generics
import qualified Data.Aeson as J



data ErpError a b = Error a | Success b deriving (Show, Eq, Ord, Typeable,Generic,Data)

instance J.ToJSON (ErpError a b)
instance J.FromJSON (ErpError a b)
$(deriveSafeCopy 0 'base ''ErpError)






