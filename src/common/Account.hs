module Account where
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as M
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co

type Name = String
type Code = String 
type Boolean = Bool

data AccountType = IncomeStatement | BalanceSheet | DisplayBalance
    deriving(Show, Enum, Bounded, Typeable, Generic)
data AccountKind = Payable | Receivable | AkExpense | AkRevenue | View | Other
    deriving (Show, Enum, Bounded, Typeable, Generic)
data Account = Account {
        name :: Name,
        code :: Code,
        aCompany :: Co.Company,
        parentAccount :: Account,
        currency :: Cu.Currency,
        kind :: AccountKind,
        acType :: AccountType,
        deferral :: Boolean,
        altCurrency :: Cu.Currency,
        reconcile :: Boolean,
        -- A list of tax auto complete move with
        -- new moves lines corresponding to those taxes
        -- if the user create a line linked to the current account
        -- and if the journal type is Expense or Revenue
        -- The above requirement is a bit too complicated
        -- Note: Need to refactor the requirement to make 
        -- it clearer.
        taxes :: [Tax],
        note :: String}
        deriving(Show,Typeable, Generic)
type DebitAccount = Account
type CreditAccount = Account
type DisplayView = String
data JournalType = General | Revenue | Situation | Expense 
        | Cash DebitAccount CreditAccount
        deriving (Show, Typeable, Generic)
data Journal = Journal {
    jName :: Name,
    jCode :: Code,
    jActive :: Boolean,
    view   :: DisplayView,
    updatePosted :: Boolean,
    journalType :: JournalType}
    deriving (Show, Typeable, Generic)
data MoveState  = Draft | Posted 
    deriving (Show, Typeable, Generic)
data Move = Move {
    mName :: Name,
    mReference :: String,
    period :: Fy.FiscalYearPeriod,
    journal :: Journal,
    effectiveDate :: UTCTime,
    postDate ::  UTCTime,
    mState :: MoveState}
    deriving (Show, Typeable, Generic)

data MoveLineState = MlDraft | MlPosted | MlValid
    deriving (Show, Typeable, Generic)
data MoveLine = MoveLine {
    mlName :: Name,
    mlReference :: String,
    mlAmount :: Amount,
    account :: Account,
    move :: Move,
    mlState :: MoveLineState,
    secondCurrency :: Cu.Currency,
    maturityDate :: UTCTime,
    reconciliation :: Maybe Integer,
    taxLines :: [Distribution]}
    deriving (Show, Typeable, Generic)

{-- XXX: This distribution is a list of amount lins on the account chart ??--}
type Distribution = String 
data Sign = Positive | Negative 
        deriving (Show, Typeable, Generic)
type Amount = Float 
data TaxCode = TaxCode {
    tcName :: Name,
    tcCode :: Code,
    tcActive  :: Boolean,
    tcCompany :: Co.Company,
    parentTC:: TaxCode,
    childrenTC :: [TaxCode],
    sum :: Amount} deriving (Show, Typeable, Generic)
    
type Sequence = String
data TaxType = PercentTaxType Float | FixedTaxType Float
    deriving (Show, Typeable, Generic)
data Tax = Tax {
 tName :: Name,
 tCode :: Code,
 description :: String,
 tActive :: Boolean,
 sequence ::Sequence,
 taxType :: TaxType,
 taxAmount :: TaxAmount,
 parentTax :: Tax,
 childrenTax :: [Tax],
 taxCompany :: Co.Company,
 invoiceAccount :: Account,
 creditNoteAccount :: Account,
 invoiceBaseCode :: TaxCode,
 invoiceBaseSign :: Sign,
 invoiceTaxCode :: TaxCode,
 invoiceTaxSign :: Sign,
 creditNoteBaseCode :: TaxCode,
 creditNoteBaseSign :: Sign,
 creditNoteTaxCode :: TaxCode,
 creditNoteTaxSign :: Sign}
    deriving(Show, Typeable, Generic)
 
data TaxAmount = Fixed Float | Percentage Float | BasisPoints Float deriving(Show, Typeable, Generic)
 
    
instance J.ToJSON JournalType
instance J.FromJSON JournalType
instance J.ToJSON Journal
instance J.FromJSON Journal        
instance J.ToJSON Account
instance J.FromJSON Account
instance J.ToJSON Tax
instance J.FromJSON Tax
instance J.ToJSON AccountType
instance J.FromJSON AccountType
instance J.ToJSON AccountKind
instance J.FromJSON AccountKind
instance J.ToJSON Sign
instance J.FromJSON Sign
instance J.ToJSON TaxCode
instance J.FromJSON TaxCode
instance J.ToJSON TaxAmount
instance J.FromJSON TaxAmount
instance J.ToJSON TaxType
instance J.FromJSON TaxType


$(deriveSafeCopy 0 'base ''JournalType)
$(deriveSafeCopy 0 'base ''Journal)
$(deriveSafeCopy 0 'base ''Account)    
$(deriveSafeCopy 0 'base ''Tax)
$(deriveSafeCopy 0 'base ''AccountType)
$(deriveSafeCopy 0 'base ''AccountKind)
$(deriveSafeCopy 0 'base ''Sign)
$(deriveSafeCopy 0 'base ''TaxCode)
$(deriveSafeCopy 0 'base ''TaxAmount)
$(deriveSafeCopy 0 'base ''TaxType)
getAccountTypes = map(\x -> (L.pack (show x),x)) ([minBound..maxBound]::[AccountType])


                 
                 
                 
