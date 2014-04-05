module TestHarness where
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
            cat <- arbitrary
            return (Pr.createUOMCategory name cat)

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

instance Arbitrary Co.Party where
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

instance Arbitrary Pr.UOM where
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

instance Arbitrary Cu.Currency where
      arbitrary = elements [
            Cu.createCurrency "AUD",
            Cu.createCurrency "USD",
            Cu.createCurrency "GBP",
            Cu.createCurrency "ROU",
            Cu.createCurrency "TST"]

instance Arbitrary Pr.Price where
     arbitrary = do
        price <- arbitrary
        curr <- arbitrary
        return (Pr.createPrice price curr)

instance Arbitrary Co.Latitude where
     arbitrary = do
        degrees <- arbitrary
        minutes <- arbitrary
        seconds <- arbitrary
        lDirec <- elements[Co.North, Co.South]
        return $ Co.createLatitude degrees minutes seconds lDirec
instance Arbitrary Co.Longitude where
    arbitrary = do
        degrees <- arbitrary
        minutes <- arbitrary
        seconds <- arbitrary
        loDirec <- elements[Co.East, Co.West]
        return $ Co.createLongitude degrees minutes seconds loDirec
instance Arbitrary Co.Coordinate where
    arbitrary = do
        lat <- arbitrary
        long <- arbitrary
        return $ Co.createCoordinate lat long
instance Arbitrary Co.GeoLocation where
     arbitrary = do
        uri <- arbitrary
        position <- arbitrary
        return $ Co.createGeoLocation uri position
instance Arbitrary Pr.Product where

instance Arbitrary Co.Company where
     arbitrary = do
        party <- arbitrary
        currency <- arbitrary
        alternateCurrencies <- orderedList
        productSet <- orderedList
        return (Co.createCompany party currency (S.fromList alternateCurrencies) (S.fromList productSet))

instance Arbitrary Co.CompanyWorkTime where
    arbitrary = do
        company <- arbitrary
        hoursPerDay <- arbitrary
        daysPerWeek <- arbitrary
        weeksPerMonth <- arbitrary
        monthsPerYear <- arbitrary
        return (Co.createCompanyWorkTime company hoursPerDay daysPerWeek weeksPerMonth monthsPerYear)

instance Arbitrary Ac.Batch where
    arbitrary = do
        time <- arbitrary
        id <- arbitrary
        return $ Ac.createBatch time id
instance Arbitrary Ac.Account where
    arbitrary = do
        name <- arbitrary
        code <- arbitrary
        company <- arbitrary
        currency <- arbitrary
        altCurrency <- suchThat arbitrary (/= currency)
        kind <- elements [Ac.Payable, Ac.Receivable, Ac.AkExpense, Ac.AkRevenue, Ac.View, Ac.Other]
        acType <- elements[Ac.IncomeStatement, Ac.BalanceSheet, Ac.DisplayBalance]
        deferral <- arbitrary
        reconcile <- arbitrary
        note <- arbitrary
        return $ Ac.createAccount name code company currency
                    kind acType deferral altCurrency reconcile note

instance Arbitrary Ac.Journal where
    arbitrary = do
        name <- arbitrary
        code <- arbitrary
        active <- arbitrary
        view <- arbitrary
        updatePosted <- arbitrary
        taxes <- orderedList
        journalType <- elements [Ac.General, Ac.Revenue, Ac.Situation, Ac.Expense, Ac.Cash]
        defaultDebitAccount <- arbitrary
        defaultCreditAccount <- arbitrary

        return $ Ac.createJournal name code active view updatePosted taxes journalType defaultDebitAccount defaultCreditAccount

instance Arbitrary Ac.TaxCode where
    arbitrary = do
        tcName <- arbitrary
        tcCode <- arbitrary
        tcActive <- arbitrary
        tcCompany <- arbitrary
        tcParent <- arbitrary
        sum <- arbitrary
        return $ Ac.createTaxCode tcName tcCode tcActive tcCompany tcParent sum
instance Arbitrary Ac.Sign where
    arbitrary = oneof [return Ac.Positive, return Ac.Negative]
instance Arbitrary Ac.Tax where
    arbitrary = do
        tName <- arbitrary
        tCode <- arbitrary
        description <- arbitrary
        active <- arbitrary
        sequence <- arbitrary
        taxType <- elements[Ac.FixedTaxType 1.0, Ac.PercentTaxType 20]
        taxAmount <- elements[Ac.Fixed 1.0, Ac.Percentage 20, Ac.BasisPoints 10]
        taxCompany <- arbitrary
        invoiceAccount <- arbitrary
        creditNoteAccount <- arbitrary
        invoiceBaseTaxCode <- arbitrary
        invoiceBaseSign <- arbitrary
        invoiceTaxCode <- arbitrary
        invoiceTaxSign <- arbitrary
        creditNoteBase <- arbitrary
        creditNoteSign <- arbitrary
        creditNoteTaxCode <- arbitrary
        creditNoteTaxSign <- arbitrary
        return $ Ac.createTax tName tCode description active
                sequence
                taxType
                taxCompany
                invoiceAccount
                creditNoteAccount
                (invoiceBaseTaxCode, invoiceBaseSign)
                (invoiceTaxCode, invoiceTaxSign)
                (creditNoteBase, creditNoteSign)
                (creditNoteTaxCode, creditNoteTaxSign)

