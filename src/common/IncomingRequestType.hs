module IncomingRequestType where

import Control.Exception
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Monad (forever)
import Control.Exception(bracket, handle, fromException)
import Control.Concurrent
import Control.Concurrent.Async(async, wait)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Acid as A
import qualified Network.WebSockets as WS
import System.Environment(getEnv)
import Data.Aeson as J
import GHC.Generics
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO

import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Map as Map
import GHC.Generics
import Data.Aeson

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


data IncomingRequestType = QueryNextSequence | Login | DeleteLogin | UpdateCategory | QueryDatabase 
		| CloseConnection
        | InsertParty
        | QueryParty
        | InsertUOM
        | QueryUOM
        | DeleteUOM
        | InsertCompany
        | DeleteCompany
        | QueryCompany        
    deriving (Show, Read, Generic, Typeable, Eq, Ord)

instance J.ToJSON IncomingRequestType
instance J.FromJSON IncomingRequestType
