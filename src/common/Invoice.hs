module Invoice where
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import qualified Data.Tree as Tr
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time

import GHC.Generics
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co
import qualified Product as Pr
import qualified Stock as St
import qualified Account as Ac
import qualified Project as Prj

type TimeInterval = Integer
data ReminderState = NotDue | OverDue (TimeInterval, InterestRate)
type Days = Integer
type InterestRate = Integer 
type InvoiceTerms = (Days, InterestRate)

class Reminder a where
    -- Compute the number of days for type a
    reminder :: a -> IO ReminderState

data InvoiceType = ManualInvoice | OnEffort | OnTimeSheet
    deriving(Show, Generic, Typeable, Eq, Ord, Data)
-- What values can the invoice state accept
-- We are borrowing the same values as the shipment state..
-- That may not be right.
data InvoiceState = Draft | Waiting | Assigned | Done | Cancel
    deriving(Show, Eq, Ord, Enum, Bounded, Typeable, Generic, Data)
data Invoice = Invoice {
        project :: Prj.Project,
        invoiceType :: InvoiceType,
        invoiceDate :: UTCTime,
        invoiceTerms :: InvoiceTerms,
        invoiceAmount :: Ac.Amount}
    deriving (Show, Generic, Typeable, Eq, Ord, Data)

numSecs :: Integer -> Integer
numSecs d = 24 * 60 * 60 * d
iDiffTime :: UTCTime -> UTCTime -> Integer 
iDiffTime a b = diffDays (utctDay a) (utctDay b)
computeReminderState :: UTCTime -> UTCTime -> 
                (Days, InterestRate) -> ReminderState
computeReminderState a b (d, r) =
    if (iDiffTime a b) > (numSecs d) 
    then 
        OverDue ((numSecs d), r)
    else 
        NotDue

instance Reminder Invoice where
    reminder a = 
        do
            curTime <- getCurrentTime
            invoiceTime <- return $ invoiceDate a
            return $ computeReminderState curTime invoiceTime 
                        (invoiceTerms a)            

data InvoiceMethod = BasedOnOrder | BasedOnShipment | Manual
    deriving(Show, Generic, Typeable, Eq, Ord, Data)
instance J.ToJSON InvoiceType
instance J.FromJSON InvoiceType
instance J.ToJSON InvoiceState
instance J.FromJSON InvoiceState
instance J.ToJSON Invoice
instance J.FromJSON Invoice
instance J.ToJSON InvoiceMethod
instance J.FromJSON InvoiceMethod
$(deriveSafeCopy 0 'base ''Invoice)
$(deriveSafeCopy 0 'base ''InvoiceType)
$(deriveSafeCopy 0 'base ''InvoiceState)
$(deriveSafeCopy 0 'base ''InvoiceMethod)
