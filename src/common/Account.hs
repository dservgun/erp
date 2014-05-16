module Account (
    Name, Code,
    Lot,
    TimeSpent,
    DaysOfWeek,
    Account,
    createAccount,
    CreditAccount,
    DebitAccount,
    JournalType(..),
    Journal, createJournal,validJournal,
    DisplayView,
    Move,
    MoveLine,
    MoveState,
    MoveLineState,
    Sign(..),
    Tax, createTax, validAccount,
    TaxCode, createTaxCode,
    TaxType(..),
    TaxAmount(..),
    Quantity,
    Amount,
    AccountKind(..),
    AccountType(..),
    Batch, createBatch
)where
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import qualified Control.Applicative as C
import qualified Data.Acid as A
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import Data.Data
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as Tr
import qualified Data.Aeson as J
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Time.Clock
import GHC.Generics
import ErpError
import qualified Currency as Cu
import Entity(EntityState)
import qualified FiscalYear as Fy
import qualified Company as Co
import qualified Data.Ratio as R
data InvalidAccountException = InvalidAccountException
    deriving (Show, Typeable, Generic, Data, Eq, Ord)
type Name = String
type Code = String
type Boolean = Bool
type Lot = String
type TimeSpent = Float
data DaysOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Enum, Bounded, Data, Typeable, Generic, Eq, Ord)
data AccountType = IncomeStatement | BalanceSheet | DisplayBalance
    deriving(Show, Enum, Bounded, Data, Typeable, Generic, Eq, Ord)
data AccountKind = Payable | Receivable | AkExpense | AkRevenue | View | Other
    deriving (Show, Enum, Bounded, Data,Typeable, Generic, Eq, Ord)
data Account = Account {
        name :: Name,
        code :: Code,
        aCompany :: Co.Company,
        currency :: Cu.Currency,
        acKind :: AccountKind,
        acType :: AccountType,
        deferral :: Boolean,
        altCurrency :: Cu.Currency,
        reconcile :: Boolean,
        note :: String,
        -- The dunning sets for the account.
        -- presumably we can have multiple
        -- dunning procedures per account?
        -- Dunning aka payment reminders
        dunningSet :: S.Set Dunning}
        deriving(Show,Typeable, Data, Generic, Eq, Ord)
type DebitAccount = ErpError ModuleError Account
type CreditAccount = ErpError ModuleError Account
type DisplayView = String
debit :: Account -> Bool
debit anAccount =
    let accType = acKind anAccount in
    case accType of
        Payable -> True
        AkExpense -> True
        Receivable -> False
        AkRevenue -> False
        _ -> False
credit anAccount =
    let
        accType = acKind anAccount in
        case accType of
            Receivable -> True
            AkRevenue -> True
            _         -> False

createAccount :: Name -> Code ->
                ErpError ModuleError Co.Company -> Cu.Currency -> AccountKind
            -> AccountType -> Boolean -> Cu.Currency
            -> Boolean -> String -> ErpError ModuleError Account
createAccount aName aCode aCompany aCurrency aKind
    aType deferral altCurrency
    reconcile note =
    case aCompany of
    Success aCom ->
            if altCurrency /= aCurrency then
                ErpError.createSuccess $
                    Account aName aCode aCom aCurrency aKind aType deferral
                    altCurrency reconcile note S.empty
            else
                ErpError.createErrorS "Account" "InvAccount" "Invalid Account"
    Error _ -> ErpError.createErrorS "Account" "InvCompany" "Invalid Company"
validAccount :: ErpError ModuleError Account -> Bool
validAccount anAccount =
    case anAccount of
        Success x -> currency x /= altCurrency x
        _ -> False
data JournalType = General | Revenue | Situation | Expense
        | Cash
        deriving (Show, Typeable, Generic, Eq, Ord, Data)
data Journal = Journal {
    jName :: Name,
    jCode :: Code,
    jActive :: Boolean,
    view   :: DisplayView,
    updatePosted :: Boolean,
    -- A list of tax auto complete move with
    -- new moves lines corresponding to those taxes
    -- if the user create a line linked to the current account
    -- and if the journal type is Expense or Revenue
    -- The above requirement is a bit too complicated
    -- Note: Need to refactor the requirement to make
    -- it clearer.
    taxes :: Maybe (Tr.Tree Tax),
    journalType :: JournalType,
    defaultDebitAccount :: Account,
    defaultCreditAccount :: Account,
    moves :: S.Set Move}
    deriving (Show, Typeable, Generic, Eq, Data)

instance Ord Journal where
    compare  t y = compare (jName t, jCode t) (jName y, jCode y)
    (<=) t y = (jName t, jCode t) <= (jName y, jCode y)

createJournal :: Name -> Code -> Bool -> DisplayView -> Bool ->
    Maybe (Tr.Tree Tax) -> JournalType -> DebitAccount -> CreditAccount ->
        ErpError ModuleError Journal
createJournal aName aCode active view updatePosted taxes jType defaultDebitAccount defaultCreditAccount =
    case jType of
        Expense -> jObject
        Revenue -> jObject
        _ ->
            case (defaultDebitAccount, defaultCreditAccount) of
                (Success x, Success y) ->
                    ErpError.createSuccess $ Journal aName aCode active view
                              updatePosted Nothing jType
                              x y S.empty
                _  -> ErpError.createErrorS "Account" "InvJournal" "Invalid Journal"
        where
            jObject =
                case (defaultDebitAccount, defaultCreditAccount) of
                    (Success x, Success y) ->
                        Success $ Journal
                        aName aCode active view updatePosted taxes jType
                        x
                        y S.empty
                    _ -> ErpError.createErrorS "Account" "InvJournal" "Invalid Journal"

validJournal :: Journal -> Bool
validJournal aJournal =
        case journalType aJournal of
            Expense -> taxes aJournal /= Nothing
            Revenue -> taxes aJournal /= Nothing
            _       -> taxes aJournal == Nothing

type ReferenceNumber = String
data MoveState  = Draft | Posted
    deriving (Show, Typeable, Generic, Eq, Ord, Data)
data Move = Move {
    mName :: Name,
    mReference :: String,
    period :: Fy.FiscalYearPeriod,
    journal :: Journal,
    effectiveDate :: UTCTime,
    postDate ::  UTCTime,
    mState :: MoveState,
    moveRefNumber :: ReferenceNumber,
    moveLines :: S.Set MoveLine}
    deriving (Show, Typeable, Generic, Eq, Ord, Data)

post :: Move -> UTCTime -> ReferenceNumber -> Move
post aMove aDate aRef =
    if balancedMove aMove aDate then
        aMove {postDate = aDate, mState = Posted, moveRefNumber = aRef}
    else
        aMove
creditMoves :: Move -> UTCTime -> S.Set MoveLine
creditMoves aMove aDate =
    let
        lines = moveLines aMove
        datedLines = S.filter (\y -> moveLineEffectiveDate y <= aDate) lines
    in
        S.filter (\x -> credit $ account x) datedLines
debitMoves :: Move -> UTCTime -> S.Set MoveLine
debitMoves aMove aDate =
    let
        lines = moveLines aMove
        datedLines = S.filter (\y -> moveLineEffectiveDate y  <= aDate) lines
    in
        S.filter (\x -> debit $ account x) datedLines

balancedMove :: Move -> UTCTime -> Bool
balancedMove aMove postDate =
    let
        creds = creditMoves aMove postDate
        debits = debitMoves aMove postDate
        creditSum = S.foldr' (\m acc -> acc + (mlAmount m)) 0 creds
        debitSum = S.foldr' (\m acc -> acc + (mlAmount m)) 0 debits
    in
        debitSum - creditSum == 0
data MoveLineState = MlDraft | MlPosted | MlValid
    deriving (Show, Typeable, Generic, Eq, Ord, Enum, Bounded, Data)
data MoveLine = MoveLine {
    mlName :: Name,
    mlReference :: String,
    mlAmount :: Amount,
    account :: Account,
    move :: Move,
    mlState :: MoveLineState,
    secondCurrency :: (Cu.Currency, Amount),
    moveLineEffectiveDate :: UTCTime,
    -- Maturity date is the limit date for the payment
    maturityDate :: UTCTime,
    reconciliation :: Maybe Integer,
    taxLines :: [Distribution]}
    deriving (Show, Typeable, Generic, Eq, Ord, Data)

{-- XXX: This distribution is a list of amount lines on the account chart ??--}
{-- This is the list of amounts for all the auto-complete taxes for the account --}
type Distribution = [(Tax, Amount)]
data Sign = Positive | Negative
        deriving (Show, Typeable, Generic, Eq , Ord, Enum, Bounded, Data)
type Amount = R.Ratio Integer
type Quantity = R.Ratio Integer
data TaxCode = TaxCode {
    tcName :: Name,
    tcCode :: Code,
    tcActive  :: Boolean,
    tcCompany :: Co.Company,
    sum :: Amount} deriving (Show, Typeable, Generic, Eq, Ord, Data)
createTaxCode :: Name -> Code -> Boolean ->
    ErpError ModuleError Co.Company -> Amount ->
    ErpError ModuleError TaxCode
createTaxCode n c a com s =
    case com of
    Success aC -> Success $ TaxCode n c a aC s
    Error _ -> ErpError.createErrorS "Acount" "INVTC" "Invalid tax code"


{-- | Add the child to a parent. If the child exists in the tree,
        return. --}
addChild :: (Tr.Tree TaxCode) -> TaxCode -> TaxCode -> Tr.Tree TaxCode
addChild aTree child parent = undefined

removeChild :: (Tr.Tree TaxCode) -> TaxCode -> TaxCode -> Tr.Tree TaxCode
removeChild aTree child parent = undefined

type Sequence = String
data TaxType = PercentTaxType Float | FixedTaxType Float
    deriving (Show, Typeable, Generic, Eq, Ord, Data)
{--
Refactoring note: credit note account
and invoice account repeat fields.
--}
type AccountCode = (Account, Sign)
type ErrorAccountCode = ErpError ModuleError (Account, Sign)
data Tax = Tax {
 tName :: Name,
 tCode :: Code,
 description :: String,
 tActive :: Boolean,
 sequence ::Sequence,
 taxType :: TaxType,
 taxCompany :: Co.Company,
 invoiceAccount :: Account,
 creditNoteAccount :: Account,
 invoiceBaseCode :: AccountCode,
 invoiceTaxCode :: AccountCode,
 creditNoteBaseCode :: AccountCode,
 creditNoteTaxCode :: AccountCode
 } deriving(Show, Typeable, Generic, Eq, Ord, Data)

createTax :: Name -> Code -> String -> Boolean -> Sequence -> TaxType ->
               ErpError ModuleError Co.Company
            -> ErpError ModuleError Account
            -> ErpError ModuleError Account
            -> ErrorAccountCode
            -> ErrorAccountCode
            -> ErrorAccountCode
            -> ErrorAccountCode -> ErpError ModuleError Tax
createTax n c s b se tt co acc1 acc2 a1 a2 a3 a4 =
    case (acc1, acc2) of
    (Success a11, Success a21) ->
        case (a1, a2, a3, a4) of
            (Success a211, Success a222, Success a333, Success a444) ->
                case co of
                    Success aCo ->
                        Success $ Tax n c s b se tt aCo a11 a21 a211 a222 a333 a444
                    Error _ -> ErpError.createErrorS "Account" "InvTax" "Invalid Tax"
            _ -> ErpError.createErrorS "Account" "InvAccount" "Invalid Acccounts"
    _               -> ErpError.createErrorS "Account" "InvAccount" "Invalid Accounts"


validTax :: Tax -> Bool
validTax aTax = validTaxType (taxType aTax)
           && (invoiceAccount aTax /= creditNoteAccount aTax)



validTaxType :: TaxType -> Bool
validTaxType = \taxType ->
    case taxType of
        PercentTaxType anAmount -> anAmount > 0.0 && anAmount < 100.00
        FixedTaxType anAmount -> True


type TaxTree = Tr.Tree Tax

{--
What does a fixed tax amount mean: Would it be possible for
a fixed tax amount to ever be greater than the amount the
tax is being applied on?
This tax is regressive.
--}
data TaxAmount = Fixed (R.Ratio Integer) | Percentage (R.Ratio Integer)
        | BasisPoints (R.Ratio Integer)
    deriving(Show, Typeable, Generic, Eq, Ord)

computeTaxAmount:: Amount -> TaxAmount -> Amount
computeTaxAmount a t =
    case t of
    Fixed aNumber -> a + aNumber
    Percentage pct -> a *  (1 + pct/100)
    BasisPoints bps -> a * (1 + bps/ 10000)

data Procedure = Procedure String deriving (Show, Typeable, Generic, Eq, Ord, Data)
type Day = Int
data Level = Level Day deriving (Show, Typeable, Generic, Eq, Ord, Data)
type BatchID = String

data Batch = Batch {
                batchDate :: UTCTime,
                batchID :: BatchID }
                deriving (Show, Typeable, Generic, Eq, Ord, Data)

createBatch :: UTCTime -> BatchID -> Batch
createBatch = Batch

createNewBatch :: BatchID => IO Batch
createNewBatch anId = do
    c <- getCurrentTime
    return $ createBatch c anId

data DunningState = DDraft | DDone deriving
    (Show, Typeable, Generic, Eq, Ord, Enum, Bounded, Data)

data Dunning = Dunning {
        line :: MoveLine,
        procedure :: Procedure,
        level :: Level,
        -- If true, the dunning is blocked..
        blocked :: Bool,
        dunningState :: DunningState
        } deriving (Show, Typeable, Generic, Eq, Ord, Data)


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

The above algorithm is a general information, to keep the audits simple,
purchases and sales are part of a batch and the inventory
is the difference between sales for a batch and purchases for the same
batch.
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





