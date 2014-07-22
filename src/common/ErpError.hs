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



module ErpError (ErpM,
    Erp(..)
    , ErpError(Error,Success),
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
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.IO.Class

data ErpError a b = Error a | Success b deriving (Show, Eq, Ord, Typeable,Generic,Data)

-- newtype ErpErrorT m a b = ErpErrorT {
--     runMaybeT :: m (ErpError a b)
-- }

data Erp a m b= Erp{runErp :: m (ErpError a b)}

-- specialized for a list of module errors and a IO monad
type ErpM= Erp [ModuleError] IO


-- instead of using the IO monad for computations with possible errors, use the Erp monad:
--
-- for example
--
-- proc :: IO (ErpError [ModuleError] Something)
-- proc= do
--      r <- something
--      someotherproc r
--      return $ Success some
--
--
-- can be used in the Erp monad as:
--
-- proc1 :: ErpT Something
-- proc1 = Erp proc
--
-- the ModuleError is a list because it is necessary to return sometime many errors instead of one.
--
instance Monad m => Monad (Erp a m) where
     return= Erp . return . Success
     mx >>= mf= Erp $ do
         x <- runErp mx
         case x of
            Error a  -> return $ Error a
            Success b -> runErp $ mf b

instance MonadTrans (Erp a) where
    lift mx= Erp $ do
        x <- mx
        return $ Success x

-- to execute an IO computation inside the Erp monad
instance MonadIO m => MonadIO (Erp a m) where
    liftIO= lift . liftIO

instance Monad m => Functor (Erp a m) where
    fmap f mx= Erp $ do
         r <- runErp mx
         case r of
           Error s   -> return  $ Error s
           Success x -> return . Success $ f x

-- The applicative instance, handles and acccumulates all possible error messages
-- and conditions using the monoid instance of a. See CreateProductNew
instance (Monoid a, Monad m) => Applicative (Erp a m) where
    pure= return
    mf <*> mx= Erp $ do
       f <- runErp mf
       x <- runErp mx
       case (f, x) of
         (Success f, Success x) -> return $ Success $ f x
         (Error s, Error s')    -> return $ Error (s <> s')
         (Success _, Error s)   -> return $ Error s
         (Error s, Success _)   -> return $ Error s


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



