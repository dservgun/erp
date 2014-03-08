module Purchase where
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

{-- These values seem to be more for the display/report and customization purposes --}
{-- XXX: Review this --}
data PurchaseLineType = Line | Comment | Subtotal | Title
    deriving(Show, Ord, Eq, Typeable, Generic)
data PurchaseState = Draft | Quotation | Confirmed | Canceled
    deriving(Show, Ord, Eq, Typeable, Generic)
data PurchaseLine = PurchaseLine {
    lineType :: PurchaseLineType,
    sequenceAllowed :: Bool,
    product :: Pr.Product,
    lineDescription :: String,
    quantity :: Ac.Quantity,
    unit :: Pr.UOM,
    unitPrice :: Ac.Amount,
    taxes :: [Ac.Tax]        
    } deriving (Show, Eq, Ord, Typeable, Generic)
data Purchase = Purchase {
    supplier :: Co.Party,
    invoiceAddress :: Co.Address,
    supplierReference :: Co.SupplierReference,
    description :: String,
    internalReference :: Co.InternalReference,
    purchaseDate :: UTCTime,
    paymentTerm :: Co.PaymentTerm,
    warehouse :: St.Location, -- St.Location.locationType = Warehouse
    currency :: Cu.Currency,
    purchaseLines :: [PurchaseLine],
    invoiceState :: In.InvoiceState,
    shipmentState :: Sh.ShipmentState,
    untaxed :: Ac.Amount,
    tax :: Ac.Amount,
    total :: Ac.Amount,
    purchaseState :: PurchaseState,
    company :: Co.Company,
    invoiceMethod :: In.InvoiceMethod,
    comments :: String,
    relatedInvoices :: [In.Invoice],
    moves :: [St.Move],
    shipments :: [Sh.Shipment],
    returnShipments ::[Sh.Shipment]}
    deriving(Eq, Ord, Typeable, Generic, Show)

lineAmount :: PurchaseLine -> Ac.Amount
lineAmount aPurchaseLine = (quantity aPurchaseLine) * (unitPrice aPurchaseLine)
    
instance J.ToJSON Purchase
instance J.FromJSON Purchase
instance J.ToJSON PurchaseLine
instance J.FromJSON PurchaseLine
instance J.ToJSON PurchaseLineType
instance J.FromJSON PurchaseLineType
instance J.ToJSON PurchaseState
instance J.FromJSON PurchaseState


$(deriveSafeCopy 0 'base ''Purchase)    
$(deriveSafeCopy 0 'base ''PurchaseLine)
$(deriveSafeCopy 0 'base ''PurchaseLineType)
$(deriveSafeCopy 0 'base ''PurchaseState)    
    
    