module TestHarness where
import qualified ErpError as ErEr
import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L
import qualified Data.Aeson as J
import qualified Company as Co
import qualified Currency as Cu
import qualified Account as Ac
import ErpServer(testServerMain)
import Control.Monad(forever, unless, liftM)
import Control.Monad.Trans (liftIO)
import Control.Exception
import Control.Concurrent
import Control.Applicative
import Control.Concurrent.Async(async, wait, cancel)
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.Time.Clock
import Data.DateTime
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La
import qualified System.Directory as SD
import Test.QuickCheck
import Product as Pr
import Text.Printf


instance Arbitrary Co.Category where
    arbitrary = do
        name <- arbitrary
        return $ Co.Category name

instance Arbitrary Pr.UOMCategory where
    arbitrary = do
            name <- arbitrary
            return (Pr.createUOMCategory name)

instance Arbitrary Pr.Attribute where
    arbitrary = do
        name <- arbitrary
        desc <- arbitrary
        return $ Pr.createAttribute name desc 


instance Arbitrary Pr.CostPriceMethod where
    arbitrary = elements [Pr.Fixed, 
                        Pr.Average Pr.LIFO,
                        Pr.Average Pr.FIFO]
instance Arbitrary Co.ContactType where
    arbitrary = elements [Co.Phone
                    , Co.Mobile
                    , Co.Fax, Co.Email
                    , Co.Website
                    , Co.Skype
                    , Co.SIP
                    , Co.IRC
                    , Co.Jabber]
instance Arbitrary Co.Contact where
    arbitrary = do
        contactType <- arbitrary
        value <- arbitrary
        return $ Co.Contact contactType value

instance Arbitrary (ErEr.ErpM Co.Party) where
    arbitrary = do
        name <- arbitrary
        addresses <- orderedList
        mapLocation <- arbitrary
        poc <- arbitrary
        primaryCategory <- arbitrary
        vcard <- arbitrary
        alternateCategories <- orderedList
        alternatePocs <- orderedList
        return $ Co.createParty name addresses mapLocation poc primaryCategory vcard
            (S.fromList alternateCategories) (S.fromList alternatePocs)

-- How do we enforce the rate and factor relationship?



instance Arbitrary (Cu.Currency) where
      arbitrary = elements [
            Cu.Currency "AUD",
            Cu.Currency "USD",
            Cu.Currency "GBP",
            Cu.Currency "ROU",
            Cu.Currency "TST"]

instance Arbitrary (ErEr.ErpM Pr.Price) where
     arbitrary = do
        price <- arbitrary
        curr <- arbitrary
        return (Pr.createPrice price curr)

instance Arbitrary  (ErEr.ErpM Co.Latitude) where
     arbitrary = do
        degrees <- arbitrary
        minutes <- arbitrary
        seconds <- arbitrary
        lDirec <- elements[Co.North, Co.South]
        return $ Co.createLatitude degrees minutes seconds lDirec

instance Arbitrary (ErEr.ErpM Pr.Dimensions) where
    arbitrary = do
        length <- arbitrary
        height <- arbitrary
        width  <- arbitrary
        weight <- arbitrary
        return $ Pr.createDimensions length width height weight


instance Arbitrary (ErEr.ErpM Co.Longitude) where
    arbitrary = do
        degrees <- arbitrary
        minutes <- arbitrary
        seconds <- arbitrary
        loDirec <- elements[Co.East, Co.West]
        return $ Co.createLongitude degrees minutes seconds loDirec
instance Arbitrary (ErEr.ErpM Co.Coordinate) where
    arbitrary = do
        lat <- arbitrary
        long <- arbitrary
        return $ Co.createCoordinate lat long
instance Arbitrary (ErEr.ErpM Co.GeoLocation) where
     arbitrary = do
        uri <- arbitrary
        position <- arbitrary
        return $ Co.createGeoLocation uri position

-- How do we enforce the rate and factor relationship?


instance Arbitrary ProductType where
    arbitrary = do
        productTypes <- elements [Pr.Goods, Pr.Assets, Pr.Services]
        return $ productTypes

instance Arbitrary (ErEr.ErpM Pr.UOM) where
    arbitrary = do
        name <- arbitrary
        symbol <- arbitrary
        category <- arbitrary
        num <- suchThat arbitrary (/= 0)
        denom <- suchThat arbitrary (/= 0)
        rounding <- arbitrary
        displayDigits <- arbitrary
        uActive <- arbitrary
        return (Pr.createUOM name symbol category num denom displayDigits rounding uActive)

instance Arbitrary (ErEr.ErpM Pr.Product) where
    arbitrary = 
        do
            productUpcCode <- arbitrary
            productDescription <- arbitrary
            productName <- arbitrary
            productType <- arbitrary
            productCategory <- arbitrary
            listPrice <- arbitrary
            listPriceUOM <- arbitrary
            costPrice <- arbitrary
            uom <- arbitrary
            costPriceMethod <- arbitrary
            attributes <- arbitrary
            dimensions <- arbitrary
            dimensionValues <- orderedList
            dimensionKeys <- orderedList
            active <- arbitrary
            return $ Pr.createProduct productUpcCode
                    productDescription
                    productName
                    productType
                    productCategory
                    (ErEr.runErp (listPrice, listPriceUOM)) costPrice costPriceMethod
                    uom
                    (S.fromList attributes)
                    active
                    dimensions 
                    (return $ Map.fromList $ zip dimensionKeys dimensionValues)
                    



--Note: well it is more convenient to add methods to 
--maniuplate a company to adding products rather
--than having us to handle more error cases.
--TODO: Fix this
instance Arbitrary (ErEr.ErpM Co.Company) where
     arbitrary = do
        party <- arbitrary
        currency <- arbitrary
        alternateCurrencies <- orderedList
        return (Co.createCompany party currency
            (S.fromList alternateCurrencies) (S.empty))

instance Arbitrary (ErEr.ErpM Co.CompanyWorkTime) where
    arbitrary = do
        company <- arbitrary
        hoursPerDay <- arbitrary
        daysPerWeek <- arbitrary
        weeksPerMonth <- arbitrary
        monthsPerYear <- arbitrary
        return (Co.createCompanyWorkTime company
            hoursPerDay (0, 8)
            daysPerWeek (0, 5)
            weeksPerMonth (0, 5)
            monthsPerYear (0, 12) )

-- Why is this computation not under the
-- ErpError monad?
instance Arbitrary Ac.Batch where
    arbitrary = do
        time <- arbitrary
        id <- arbitrary
        return $ Ac.createBatch time id

instance Arbitrary (ErEr.ErpM Ac.Account) where
    arbitrary = do
        name <- arbitrary
        code <- arbitrary
        company <- arbitrary
        currency <- arbitrary
        altCurrency <- suchThat arbitrary (/= currency)
        acKind <- elements [Ac.Payable, Ac.Receivable, Ac.AkExpense, Ac.AkRevenue, Ac.View, Ac.Other]
        acType <- elements[Ac.IncomeStatement, Ac.BalanceSheet, Ac.DisplayBalance]
        deferral <- arbitrary
        reconcile <- arbitrary
        note <- arbitrary
        return (Ac.createAccount name code 
                    company currency
                    acKind acType
                    deferral altCurrency 
                    reconcile note)

instance Arbitrary (ErEr.ErpM Ac.Journal) where
    arbitrary = do
        name <- arbitrary
        code <- arbitrary
        active <- arbitrary
        view <- arbitrary
        updatePosted <- arbitrary
        journalType <- elements [Ac.General, Ac.Revenue, Ac.Situation, Ac.Expense, Ac.Cash]
        defaultDebitAccount <- arbitrary
        defaultCreditAccount <- arbitrary
        return $ Ac.createJournal name code active view updatePosted Nothing journalType defaultDebitAccount defaultCreditAccount

instance Arbitrary (ErEr.ErpM Ac.TaxCode) where
    arbitrary = do
        tcName <- arbitrary
        tcCode <- arbitrary
        tcActive <- arbitrary
        tcCompany <- arbitrary
        sum <- arbitrary
        return $ Ac.createTaxCode tcName tcCode tcActive tcCompany sum

instance Arbitrary (Ac.Sign) where
    arbitrary = oneof [return Ac.Positive,
                    return Ac.Negative]


