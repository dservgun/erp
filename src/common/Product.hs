module Product (
      UOM
    , createUOM
    , validUOM
    , UOMCategory
    , createUOMCategory
    , Price
    , createPrice
    , PriceUOM
    , ProductError
    , ErpError(..)
    , Product
    , Dimensions
    , createDimensions
    , validDimensions
    )
where
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import Data.Tree as Tr
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Currency as Cu
import Data.Ratio
import ErpError


data ProductError = ProductError {eMsg :: String} deriving Show

data InvalidUOMException = InvalidUOMException deriving Show

data UOMCategory = UOMCategory {catName :: String}
    deriving(Show, Generic, Data, Typeable, Eq, Ord)

findCategory :: UOMCategory -> Tr.Tree UOMCategory -> ErpError
                            ModuleError UOMCategory
findCategory aCat aTree =
    case r of
    [] -> ErpError.Error $ ModuleError "Product" "CatNotFound"
                                "Category not found"

    [h] -> ErpError.Success h
    _ -> ErpError.Error $ ModuleError "Product" "DupCat"
                    "Duplicate categories"
    where
        r = filter (\x -> x == aCat) (Tr.flatten aTree)
createUOMCategory :: String -> UOMCategory
createUOMCategory = UOMCategory
createRootCategory :: UOMCategory -> Tr.Tree UOMCategory
createRootCategory aCat = Tr.Node aCat []
updateCategoryTree :: UOM -> Tr.Tree UOMCategory -> UOM
updateCategoryTree aUOM aCat =
    aUOM {
           category = aCat
         }


{-- UOM defines the unit of measure for the product --}
data UOM =  UOM {
        name :: String,
        symbol :: String,
        category :: Tr.Tree UOMCategory,
        rate :: Ratio Integer,
        factor :: Ratio Integer,
        rounding :: Int,
        displayDigits :: Int,
        uActive :: Bool}
    deriving (Show, Generic, Data, Typeable,Eq)
instance Ord UOM where
    compare  t y = compare (name t, symbol t) (name y, symbol y)
    (<=) t y = (name t, symbol t) <= (name y, symbol y)

type Numerator = Integer
type Denominator = Integer
createUOM :: String -> String -> UOMCategory -> Numerator -> Denominator -> Int -> Int ->
    Bool -> ErpError ProductError UOM
createUOM name symbol category num den rounding displayDigits active =
        if den == 0 then
            Error $ ProductError "InvalidUOM"
        else
            Success $ UOM name symbol (createRootCategory category) (num % den) (den % num) rounding
            displayDigits active


validUOM :: UOM -> Bool
validUOM aUOM = ((rate aUOM) * (factor aUOM)) == 1

data Price = Price {p :: Float,
                    cu :: Cu.Currency}
            deriving (Show, Data, Generic, Typeable, Eq, Ord)
createPrice :: Float -> Cu.Currency -> Price
createPrice = Price
type PriceUOM = (Price, UOM)
data CPMType = LIFO | FIFO
    deriving (Show, Generic, Data, Typeable, Eq, Ord)
data CostPriceMethod = Fixed | Average CPMType
    deriving(Show, Generic, Data, Typeable, Eq, Ord)
data ProductType = Goods | Assets | Services
    deriving(Show, Generic, Data, Typeable, Eq, Ord)
type UPCCode = Maybe String
data Attribute = Attribute {attributeName :: String, attrDescription:: String}
    deriving (Show, Generic, Data, Typeable, Eq, Ord)
type Weight = Float
type Height = Float
type Length = Float
type Width = Float
-- This needs to be its own module
type Image = String

-- lengthD because length is sort of reserved in haskell.
data Dimensions =
    Dimensions {lengthD :: Length, width :: Width, height :: Height, weight :: Weight}
    deriving (Show, Generic, Data, Typeable, Eq, Ord, Read)

{--
create dimensions for the product. Sizes will differ for different type
of product sizes.
--}
createDimensions :: Length -> Width -> Height -> Weight ->
    ErpError ProductError Dimensions
createDimensions a b c w =
    if a > 0 && b > 0  && c > 0 && w > 0 then
        Success $ Dimensions a b c w
    else
        Error $ ProductError $ "Invalid dimensions "  ++ (show a) ++ "," ++
            (show b) ++ "," ++
            (show c) ++ ", " ++ (show w)

validDimensions :: Dimensions -> Bool
validDimensions d  = a > 0 && b > 0 && c > 0 && w > 0
                    where
                        a = lengthD d
                        b = width d
                        c = height d
                        w = weight d
type ProductAudit = (UTCTime, Product, PriceUOM)
{-- |
  The size here represents a pair of multiple and the uom. For example,
  if a product is packaged in 1 liter, 5 liter and 20 liter packages,
  then liter is the UOM and the size for the product could be
  one of
  (1, Liter)
  (5, Liter)
  (20, Liter)

 |--}
type Size = (Int, UOM)

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
        -- The key is the string of the Size type
        -- How to create custom maps such that
        -- we can create json from them?
        -- Show interface should have sufficed:
        -- Need to find out
        dimensionsMap :: M.Map String (Dimensions, Image),
        parentProduct :: Product,
        productHistory:: [ProductAudit]}
        deriving (Data,Show, Generic, Typeable, Eq, Ord)

addDimensions :: Product -> Size -> Dimensions -> Image -> Product
addDimensions aProduct aSize dim image =
    aProduct {dimensionsMap =
        M.insert (show aSize) (dim, image) (dimensionsMap aProduct)}

deleteDimensions :: Product -> Size -> Product
deleteDimensions aProduct aSize = aProduct {
        dimensionsMap = M.delete (show aSize) (dimensionsMap aProduct)
        }

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
