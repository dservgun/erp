module Product (
    UOM, createUOM, validUOM,
    UOMCategory, createUOMCategory, 
    Price, createPrice,
    PriceUOM,
    Product)
    

where
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



data UOMCategory = UOMCategory {catName :: String,
                            parentCat :: Maybe UOMCategory}
    deriving(Show, Generic, Typeable, Eq, Ord)
createUOMCategory :: String -> Maybe UOMCategory -> UOMCategory
createUOMCategory aString aCat = UOMCategory aString aCat
{-- UOM defines the unit of measure for the product --}
data UOM = UOM {
        name :: String,
        symbol :: String,
        category :: UOMCategory,
        rate :: Float,
        factor :: Float,
        rounding :: Int,
        displayDigits :: Int,
        uActive :: Bool}
    deriving (Show, Generic, Typeable,Eq, Ord)

createUOM :: String -> String -> UOMCategory -> Float -> Int -> Int -> Bool -> UOM
createUOM name symbol category rate rounding displayDigits active = 
        UOM name symbol category adRate (1/adRate) rounding displayDigits active
        where
            adRate = case (abs rate) of 
                        0.0 -> 1.0
                        _ -> abs rate
validUOM :: UOM -> Bool
validUOM aUOM = True

data Price = Price {p :: Float,
                    cu :: Cu.Currency}
            deriving (Show, Generic, Typeable, Eq, Ord)
createPrice :: Float -> Cu.Currency -> Price
createPrice p cu = Price p cu
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
        productCategory :: UOMCategory,
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
        


instance J.ToJSON UOMCategory
instance J.FromJSON UOMCategory
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
$(deriveSafeCopy 0 'base ''UOMCategory)
$(deriveSafeCopy 0 'base ''Price)
$(deriveSafeCopy 0 'base ''Product)
$(deriveSafeCopy 0 'base ''ProductType)
$(deriveSafeCopy 0 'base ''Attribute)
$(deriveSafeCopy 0 'base ''CostPriceMethod)    
$(deriveSafeCopy 0 'base ''CPMType)    
$(deriveSafeCopy 0 'base ''Dimensions)