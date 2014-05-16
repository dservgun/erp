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
import Data.Data
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
import qualified Carrier as Ca

-- Similar to Purchase Line Type with these constructors being used for
-- reporting purposes
{--| Who pays for shipping
   | If the product is damaged, then
   | the shipper pays for the return and the replacement
   | If the customer doesnt like the product,
   | and returns, then shipper pays
   | So the customer will pay probably when
   | the customer acknowledges a mistake in his
   | buying habits : almost never
--}
data ReturnShipmentCode = Shipper | Receiver
    deriving (Show, Eq, Ord, Typeable, Generic, Enum, Data)
data ReturnReason = DefectiveShipment | DefectiveProduct | UserHatesIt | UserWhim
    deriving (Show, Eq, Ord, Typeable, Generic, Enum, Data)
data ReturnPolicy  = Return {
        returnReason :: ReturnReason,
        returnComments :: L.Text,
        returnType :: ReturnShipmentCode
     }
    deriving (Show, Eq, Ord, Typeable, Generic, Data)

{--| Was the return a user whim (almost rarely)

--}
userWhim :: Sale -> Bool
userWhim aSale = (returnReason $ returnPolicy aSale) == UserWhim
updateReturnPolicy :: ReturnPolicy -> ReturnReason -> ReturnPolicy
updateReturnPolicy a r = case r of
                        UserWhim -> a {returnType = Receiver}
                        _ -> a {returnType = Shipper}

data SaleLineType = Line | Title | Comment | Subtotal
    deriving(Show, Eq, Ord, Typeable, Generic, Enum, Bounded, Data)
data SaleLine = SaleLine {
    lineType :: SaleLineType,
    sequence :: Bool,
    batch :: Ac.Batch,
    product :: Pr.Product,
    lineDescription :: L.Text, -- Additional comments.
    quantity :: Ac.Quantity,
    unit :: Pr.UOM,
    unitPrice :: Ac.Amount,
    taxes :: [Ac.Tax]
    } deriving (Show, Ord, Eq, Typeable, Generic, Data)
data SaleState = Draft | Quotation | Confirmed | Processed | Canceled
    deriving (Show, Ord, Eq,Typeable, Generic, Enum, Bounded, Data)
data Sale = Sale {
        party :: Co.Party,
        invoiceAddress :: Co.Address,
        shipmentAddress :: Co.Address,
        description :: L.Text,
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
        comments :: L.Text,
        relatedInvoices :: [In.Invoice],
        relatedMoves ::  [Ac.Move],
        shipments :: [Sh.Shipment],
        returns :: [Sh.Shipment],
        carrier :: Ca.Carrier,
        shipmentCostMethod :: Sh.ShipmentCostMethod,
        returnPolicy :: ReturnPolicy,
        saleSupply :: Bool -- Generate a supply request regardless of stock levels.
        } deriving (Show, Ord, Eq, Typeable, Generic)

createSale :: Co.Party -> Co.Address ->
                Co.Address -> L.Text -> Co.InternalReference
                -> UTCTime -> Co.PaymentTerm ->
                St.Location ->
                Cu.Currency -> [SaleLine] -> In.InvoiceState ->
                Sh.ShipmentState ->
                Ac.Amount -> Ac.Amount ->
                SaleState -> Co.Company ->
                In.InvoiceMethod -> Sh.ShipmentMethod ->
                L.Text ->
                [In.Invoice] -> [Ac.Move] -> [Sh.Shipment] ->
                [Sh.Shipment] ->
                Ca.Carrier -> Sh.ShipmentCostMethod ->
                ReturnPolicy -> Bool -> Sale
createSale = Sale
addShipment :: Sale -> Sh.Shipment -> Sale
addShipment aSale aShipment = aSale {shipments = aShipment : (shipments aSale)}

hasReturns :: Sale -> Bool
hasReturns  = (\x -> [] == returns x)

-- When the shipping cost is greater or equal
-- to the untaxed amount. It can happen in
-- international shipments of sample size packages
tooExpensiveToShip :: Sale -> Bool
tooExpensiveToShip aSale = (untaxedAmount aSale) <= (shippingCosts aSale)
-- shipments - returns
partialReturns :: [Sh.Shipment] -> [Sh.Shipment] -> Bool
partialReturns a b = (abs $ (length a - length b)) > 0


validSaleInvariant :: Sale -> Bool
-- Add all the rules here
validSaleInvariant aSale =
    (length $ shipments aSale) > (length $ returns aSale)


-- Shipping costs are computed in two ways:
-- One where the receiving party is responsible
-- for the returns, the other where the
-- sender is responsible for it.

shippingCosts :: Sale -> Ac.Amount
shippingCosts aSale =
    if userWhim aSale then
        Sh.computeCosts $ shipments aSale
    else
        Sh.computeCosts $ (shipments aSale) ++ (returns aSale)



instance J.ToJSON SaleLineType
instance J.FromJSON SaleLineType
instance J.ToJSON SaleLine
instance J.FromJSON SaleLine
instance J.ToJSON Sale
instance J.FromJSON Sale
instance J.ToJSON SaleState
instance J.FromJSON SaleState
instance J.ToJSON ReturnShipmentCode
instance J.FromJSON ReturnShipmentCode
instance J.ToJSON ReturnReason
instance J.FromJSON ReturnReason
instance J.ToJSON ReturnPolicy
instance J.FromJSON ReturnPolicy


$(deriveSafeCopy 0 'base ''SaleLineType)
$(deriveSafeCopy 0 'base ''SaleLine)
$(deriveSafeCopy 0 'base ''Sale)
$(deriveSafeCopy 0 'base ''SaleState)
$(deriveSafeCopy 0 'base ''ReturnReason)
$(deriveSafeCopy 0 'base ''ReturnShipmentCode)
$(deriveSafeCopy 0 'base ''ReturnPolicy)
