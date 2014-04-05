module Currency (
    Currency,
    createCurrency,
    usd,
    eur,
    cad
)
where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import Network.URL
import qualified Data.Ratio as R
import qualified Country as Cou

data Currency = Currency String
    deriving (Show, Typeable, Data, Generic, Eq, Ord)

data Exchange = Exchange{from :: Currency,
                 to ::  Currency,
                 rate :: R.Ratio Integer}

type Amount = R.Ratio Integer

createCurrency = Currency
usd = Currency "USD"
eur = Currency "EUR"
cad = Currency "CAD"

{-- Convert one currency into another --}
{-- Check for the currency rate, for example --}

convert :: (Currency, Amount) -> Exchange -> (Currency, Amount)

{-- Exchange is doing a straight exchange, so from and to are specified --}
convert (c1, a) exchange  =
    if c1 == (Currency.from exchange) then
        (Currency.to exchange, a * (Currency.rate exchange))
    else
        (c1, a)


$(deriveSafeCopy 0 'base ''Currency)

instance J.ToJSON Currency
instance J.FromJSON Currency
