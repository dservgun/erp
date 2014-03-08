module Shipment where
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
import qualified Carrier as Ca

data ShipmentState = Draft | Waiting | Assigned | Done | Cancel
    deriving(Show, Eq, Ord, Enum, Bounded, Typeable, Generic)
-- Avoid name collision
data ShipmentType = Supplier | Customer | Internal | InventoryShipment
    deriving (Show, Eq, Ord, Typeable, Generic)
data Shipment = Shipment {
        incoming :: [St.Move],
        inventory :: [St.Move],
        shipmentState :: ShipmentState, 
        shipmentType :: ShipmentType, 
        carrier :: Ca.Carrier,
        cost :: Ac.Amount,
        currency :: Cu.Currency 
        } deriving (Show, Eq, Ord, Typeable, Generic)
        

data InventoryLine = InventoryLine {
            product :: Pr.Product,
            quantity :: Ac.Quantity}
        deriving (Show, Eq, Ord, Typeable, Generic)
        
data Inventory = Inventory {
       storageLocation :: St.LocationType,
       lostAndFound :: St.LocationType,
       inventoryLine :: [InventoryLine]
       } deriving (Show, Eq, Ord, Typeable, Generic)
       
instance J.ToJSON ShipmentState
instance J.FromJSON ShipmentState
instance J.ToJSON ShipmentType
instance J.FromJSON ShipmentType
instance J.ToJSON Shipment
instance J.FromJSON Shipment

$(deriveSafeCopy 0 'base ''ShipmentState)
$(deriveSafeCopy 0 'base ''ShipmentType)
$(deriveSafeCopy 0 'base ''Shipment)        


