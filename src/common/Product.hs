module Product where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Currency as Cu



data Category = UOMCategory {catName :: String,
                            parentCat :: Category}
    deriving(Show, Generic, Typeable, Eq, Ord)

{-- UOM defines the unit of measure for the product --}
data UOM = UOM {
        name :: String,
        symbol :: String,
        category :: Category,
        rate :: Float,
        factor :: Float,
        rounding :: Int,
        displayDigits :: Int,
        uActive :: Bool}
    deriving (Show, Generic, Typeable,Eq, Ord)
    
    
data Price = Price {p :: Float,
                    cu :: Cu.Currency}
            deriving (Show, Generic, Typeable, Eq, Ord)

type PriceUOM = (Price, UOM)
data CPMType = LIFO | FIFO
    deriving (Show, Generic, Typeable, Eq, Ord)
data CostPriceMethod = Fixed | Average CPMType
    deriving(Show, Generic, Typeable, Eq, Ord)
data ProductType = Goods | Assets | Services
    deriving(Show, Generic, Typeable, Eq, Ord)
type UPCCode = Maybe String    
data Attribute = Attribute {attributeName :: String, attrDescription:: String}
    deriving (Show, Generic, Typeable, Eq, Ord)
type Weight = Float    
data Dimensions = Dimensions {length :: Float, width :: Float, height :: Float, weight :: Weight}    
    deriving (Show, Generic, Typeable, Eq, Ord)
type ProductAudit = (UTCTime, Product, PriceUOM) 
   
data Product = Product {
        productUPCCode :: UPCCode,
        productDescription :: String,
        productName :: String,
        productType :: ProductType,
        productCategory :: Category,
        listPrice :: PriceUOM,
        costPrice :: PriceUOM,
        costPriceMethod :: CostPriceMethod,
        defaultUOM :: UOM,
        attributes :: S.Set Attribute,
        active :: Bool,
        dimensions :: Dimensions,
        parentProduct :: Product,
        productHistory:: [ProductAudit]}
        deriving (Show, Generic, Typeable, Eq, Ord)
        

validUOM :: UOM -> Bool
validUOM (UOM _ _ _ rate factor _ _ active) =
        active && (rate - (1/factor) < epsilon)
        where 
            epsilon = 0.00001

instance J.ToJSON Category
instance J.FromJSON Category
instance J.ToJSON UOM
instance J.FromJSON UOM
instance J.ToJSON Price
instance J.FromJSON Price
instance J.ToJSON CostPriceMethod
instance J.FromJSON CostPriceMethod
instance J.ToJSON Product
instance J.FromJSON Product
instance J.ToJSON ProductType
instance J.FromJSON ProductType
instance J.ToJSON Attribute
instance J.FromJSON Attribute
instance J.ToJSON CPMType
instance J.FromJSON CPMType
instance J.ToJSON Dimensions
instance J.FromJSON Dimensions



$(deriveSafeCopy 0 'base ''UOM)
$(deriveSafeCopy 0 'base ''Category)
$(deriveSafeCopy 0 'base ''Price)
$(deriveSafeCopy 0 'base ''Product)
$(deriveSafeCopy 0 'base ''ProductType)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''CostPriceMethod)    
$(deriveSafeCopy 0 'base ''CPMType)    
$(deriveSafeCopy 0 'base ''Dimensions)