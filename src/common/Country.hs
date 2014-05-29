module Country where

import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import Network.URL


type IsoCodeAlpha2 = String
data Country = Country { isoCode :: IsoCodeAlpha2, 
        name :: String,
        url :: String,
        description :: String}    
    deriving (Show, Typeable, Generic, Eq, Ord)
data SubdivisionType = Emirate | State | Province | District | Other
            deriving (Show, Typeable, Generic, Eq, Ord)
data Subdivision = Subdivision {sdivName :: String,
                                sdivType :: SubdivisionType}
                                deriving (Show, Typeable, Generic, Eq, Ord)
data CountrySubDivision = CountrySubdivision {
            parentCountry :: Country,
            subdivision :: Subdivision
        } deriving (Show, Typeable, Generic, Eq, Ord)

getURL :: Country -> Maybe URL
getURL (Country _ _ url _) = importURL url

updateURL :: Country -> URL -> Country 
updateURL aCountry aURL = aCountry {url = exportURL aURL}

        
instance J.ToJSON Subdivision 
instance J.FromJSON Subdivision
instance J.ToJSON SubdivisionType
instance J.FromJSON SubdivisionType
instance J.ToJSON CountrySubDivision        
instance J.ToJSON Country
instance J.FromJSON Country
$(deriveSafeCopy 0 'base ''Country)
$(deriveSafeCopy 0 'base ''Subdivision)
$(deriveSafeCopy 0 'base ''SubdivisionType)
$(deriveSafeCopy 0 'base ''CountrySubDivision)