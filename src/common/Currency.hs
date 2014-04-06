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


{--
    The exchange helps us determine how much of the quoteC is needed
    to buy base currency.Also,
    (baseC, quoteC) -> rate ~= (quoteC, baseC, (1/rate))
    The above relation is only approximate.
--}
data Exchange = Exchange {baseC :: Currency
                , quoteC ::  Currency
                , rate :: R.Ratio Integer
                , date :: UTCTime}

type Amount = R.Ratio Integer

createCurrency = Currency
usd = Currency "USD"
eur = Currency "EUR"
cad = Currency "CAD"


convert :: (Currency, Amount) -> Exchange -> (Currency, Amount)

{-- Exchange is doing a straight exchange, so from and to are specified --}
convert (c1, a) exchange  =
    if c1 == (Currency.baseC exchange) then
        (Currency.quoteC exchange, a * (Currency.rate exchange))
    else
        (c1, a)


$(deriveSafeCopy 0 'base ''Currency)

instance J.ToJSON Currency
instance J.FromJSON Currency
