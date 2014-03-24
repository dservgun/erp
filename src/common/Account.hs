module Account (
    Name, Code,
    Lot,
    TimeSpent,
    DaysOfWeek,
    AccountType,
    AccountKind,
    Account,
    CreditAccount,
    DebitAccount,
    JournalType,
    Journal,
    DisplayView,
    Move,
    MoveLine,
    MoveState,
    MoveLineState,
    Tax,
    TaxCode,
    TaxType,
    Quantity,
    Amount,
    Batch, createBatch
)where
import Control.Monad.State
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

type Name = String
type Code = String 
type Boolean = Bool
type Lot = String
type TimeSpent = Float
data DaysOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Enum, Bounded, Typeable, Generic, Eq, Ord)
data AccountType = IncomeStatement | BalanceSheet | DisplayBalance
    deriving(Show, Enum, Bounded, Typeable, Generic, Eq, Ord)
data AccountKind = Payable | Receivable | AkExpense | AkRevenue | View | Other
    deriving (Show, Enum, Bounded, Typeable, Generic, Eq, Ord)
data Account = Account {
        name :: Name,
        code :: Code,
        aCompany :: Co.Company,
        currency :: Cu.Currency,
        kind :: AccountKind,
        acType :: AccountType,
        deferral :: Boolean,
        altCurrency :: Cu.Currency,
        reconcile :: Boolean,
        parent :: Account,
        -- A list of tax auto complete move with
        -- new moves lines corresponding to those taxes
        -- if the user create a line linked to the current account
        -- and if the journal type is Expense or Revenue
        -- The above requirement is a bit too complicated
        -- Note: Need to refactor the requirement to make 
        -- it clearer.
        taxes :: [Tax],
        note :: String}
        deriving(Show,Typeable, Generic, Eq, Ord)
type DebitAccount = Account
type CreditAccount = Account
type DisplayView = String

data JournalType = General | Revenue | Situation | Expense 
        | Cash DebitAccount CreditAccount
        deriving (Show, Typeable, Generic, Eq, Ord)
data Journal = Journal {
    jName :: Name,
    jCode :: Code,
    jActive :: Boolean,
    view   :: DisplayView,
    updatePosted :: Boolean,
    journalType :: JournalType}
    deriving (Show, Typeable, Generic, Eq, Ord)
data MoveState  = Draft | Posted 
    deriving (Show, Typeable, Generic, Eq, Ord)
data Move = Move {
    mName :: Name,
    mReference :: String,
    period :: Fy.FiscalYearPeriod,
    journal :: Journal,
    effectiveDate :: UTCTime,
    postDate ::  UTCTime,
    mState :: MoveState}
    deriving (Show, Typeable, Generic, Eq, Ord)

data MoveLineState = MlDraft | MlPosted | MlValid
    deriving (Show, Typeable, Generic, Eq, Ord, Enum, Bounded)
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
    deriving (Show, Typeable, Generic, Eq, Ord)

{-- XXX: This distribution is a list of amount lines on the account chart ??--}
type Distribution = String 
data Sign = Positive | Negative 
        deriving (Show, Typeable, Generic, Eq , Ord, Enum, Bounded)
type Amount = Float 
type Quantity = Float
data TaxCode = TaxCode {
    tcName :: Name,
    tcCode :: Code,
    tcActive  :: Boolean,
    tcCompany :: Co.Company,
    tcParent :: TaxCode,
    sum :: Amount} deriving (Show, Typeable, Generic, Eq, Ord)

type Sequence = String
data TaxType = PercentTaxType Float | FixedTaxType Float
    deriving (Show, Typeable, Generic, Eq, Ord)
data Tax = Tax {
 tName :: Name,
 tCode :: Code,
 description :: String,
 tActive :: Boolean,
 sequence ::Sequence,
 taxType :: TaxType,
 taxAmount :: TaxAmount,
 taxCompany :: Co.Company,
 parTax :: Tax,
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
    deriving(Show, Typeable, Generic, Eq, Ord)
type TaxTree = Tr.Tree Tax 
data TaxAmount = Fixed Float | Percentage Float | BasisPoints Float 
    deriving(Show, Typeable, Generic, Eq, Ord)
 
computTaxAmount:: Amount -> TaxAmount -> Amount
computTaxAmount a t = 
    case t of
    Fixed aNumber -> a + aNumber
    Percentage pct -> a *  (1 + pct/100)
    BasisPoints bps -> a * (1 + bps/ 10000)
    
data Procedure = Proceure String deriving (Show, Typeable, Generic, Eq, Ord)
type Day = Int
data Level = Level Day deriving (Show, Typeable, Generic, Eq, Ord)
data DunningState = DDraft | DDone deriving (Show, Typeable, Generic, Eq, Ord, Enum, Bounded)
type BatchID = String
data Batch = Batch {
                batchDate :: UTCTime,
                batchID :: BatchID }
                deriving (Show, Typeable, Generic, Eq, Ord)
                
createBatch aDate anId = Batch aDate anId

createNewBatch anId = do
    c <- getCurrentTime
    return $ createBatch c anId

data Dunning = Dunning {
        line :: MoveLine,
        procedure :: Procedure,
        level :: Level,
        -- If true, the dunning is blocked..
        blocked :: Bool,
        dunningState :: DunningState
        } deriving (Show, Typeable, Generic, Eq, Ord)

printDunningLetter :: Dunning -> L.Text
printDunningLetter = L.pack . show

getAccountTypes = map(\x -> (L.pack (show x),x)) ([minBound..maxBound]::[AccountType])
{--
computing inventory costs by FIFO method, 
apparently LIFO is a US tax haven.
The FIFO works as follows:
Date No. Units Price 
d1 100          1
d2 200          10
d3 300          11

Number of units sold:
d4 20           
d5 140
d6 100

cost of d4 is 20 * 1
cost d5 is (80 * 1 + 60 * 10)
cost d6 is (100 * 10)
inventory cost between a period needs to adjust the journal accordingly
--}



instance J.ToJSON Dunning
instance J.FromJSON Dunning
instance J.ToJSON DunningState
instance J.FromJSON DunningState
instance J.ToJSON Procedure
instance J.FromJSON Procedure
instance J.ToJSON Level
instance J.FromJSON Level        
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
instance J.ToJSON Move
instance J.FromJSON Move
instance J.ToJSON MoveState
instance J.FromJSON MoveState
instance J.ToJSON MoveLine
instance J.FromJSON MoveLine
instance J.ToJSON MoveLineState
instance J.FromJSON MoveLineState
instance J.ToJSON Sign
instance J.FromJSON Sign
instance J.ToJSON TaxCode
instance J.FromJSON TaxCode
instance J.ToJSON TaxAmount
instance J.FromJSON TaxAmount
instance J.ToJSON TaxType
instance J.FromJSON TaxType
instance J.ToJSON Batch
instance J.FromJSON Batch
$(deriveSafeCopy 0 'base ''Dunning)
$(deriveSafeCopy 0 'base ''DunningState)
$(deriveSafeCopy 0 'base ''Procedure)
$(deriveSafeCopy 0 'base ''Level)
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

$(deriveSafeCopy 0 'base ''Move)
$(deriveSafeCopy 0 'base ''MoveState)
$(deriveSafeCopy 0 'base ''MoveLine)
$(deriveSafeCopy 0 'base ''MoveLineState)
$(deriveSafeCopy 0 'base ''Batch)



                 
                 
                 
