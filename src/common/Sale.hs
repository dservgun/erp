module Sale where
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Tree as Tr
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co
import qualified Product as Pr
import qualified Stock as St
import qualified Account as Ac
import qualified Invoice as In
import qualified Shipment as Sh

-- Similar to Purchase Line Type with these constructors being used for 
-- reporting purposes
data SaleLineType = Line | Title | Comment | Subtotal
    deriving(Show, Eq, Ord, Typeable, Generic, Enum, Bounded)
data SaleLine = SaleLine {
    lineType :: SaleLineType,
    sequence :: Bool,
    product :: Pr.Product,    
    lineDescription :: String, -- Additional comments.
    quantity :: Ac.Quantity,
    unit :: Pr.UOM,
    unitPrice :: Ac.Amount,
    taxes :: [Ac.Tax]
    } deriving (Show, Ord, Eq, Typeable, Generic)
data SaleState = Draft | Quotation | Confirmed | Processed | Canceled 
    deriving (Show, Ord, Eq,Typeable, Generic, Enum, Bounded)
data Sale = Sale {
        party :: Co.Party,
        invoiceAddress :: Co.Address,
        shipmentAddress :: Co.Address,
        description :: String,
        reference :: Co.InternalReference,
        saleDate :: UTCTime,
        paymentTerm :: Co.PaymentTerm,
        warehouse :: St.Location,
        currency :: Cu.Currency,
        saleLines :: [SaleLine],
        invoiceState :: In.InvoiceState,
        shipmentState :: Sh.ShipmentState,
        untaxedAmount :: Ac.Amount,
        taxAmount :: Ac.Amount,
        state :: SaleState,
        company :: Co.Company,
        invoiceMethod :: In.InvoiceMethod,
        shipmentMethod :: Sh.ShipmentMethod,
        comments :: String,
        relatedInvoices :: [In.Invoice],
        relatedMoves ::  [Ac.Move],
        shipments :: [Sh.Shipment],
        returnShipments :: [Sh.Shipment]
        } deriving (Show, Ord, Eq, Typeable, Generic)
        
        
        
        
instance J.ToJSON SaleLineType
instance J.FromJSON SaleLineType
instance J.ToJSON SaleLine
instance J.FromJSON SaleLine
instance J.ToJSON Sale
instance J.FromJSON Sale
instance J.ToJSON SaleState
instance J.FromJSON SaleState


$(deriveSafeCopy 0 'base ''SaleLineType)
$(deriveSafeCopy 0 'base ''SaleLine)    
$(deriveSafeCopy 0 'base ''Sale)
$(deriveSafeCopy 0 'base ''SaleState)