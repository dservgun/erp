
import qualified Data.Map as Map
import qualified ErpModel as M
import qualified Login as L
import qualified Data.Aeson as J
import qualified Company as Co
import qualified Currency as Cu
import ErpServer(testServerMain)
import Control.Monad(forever, unless)
import Control.Monad.Trans (liftIO)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async(async, wait, cancel)
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as En
import qualified Data.Text.Lazy as La
import qualified System.Directory as SD
import Test.QuickCheck
import Product as Pr
import Text.Printf

testEmail = "test@test.org"
createQueryDatabaseRequest login aPayload = 
    encode $ toJSON $ M.Request M.queryDatabaseConstant login $ En.decodeUtf8 aPayload

createLoginRequest login aPayload  = encode( toJSON (M.Request M.loginConstant login $ En.decodeUtf8 aPayload))

createCategoryRequest login aPayload = encode $ toJSON $ M.Request M.categoryConstant 
        login $ En.decodeUtf8 aPayload
createCloseConnection login aPayload = 
    encode $ toJSON $ M.Request M.closeConnection login $ En.decodeUtf8 aPayload


processResponse aRequest@(M.Request entity email payload) = T.pack $ show aRequest
parseMessage :: WS.Connection-> IO ()
parseMessage conn = do
    msg <- WS.receiveData conn    
    let 
        r = J.decode $ En.encodeUtf8 $ La.fromStrict msg
    case r of 
        Just aRequest -> do
            case M.requestEntity aRequest of         
                "CloseConnection" -> do
                    T.putStrLn "Received :: "
                    WS.sendClose conn ("Closing" :: T.Text)
                _ -> do 
                    T.putStrLn "Received ::" 
                    T.putStrLn $ processResponse aRequest
 
        _ -> throw M.InvalidRequest

testLogin = L.Login "test@test.org" True        

loginTest :: Int -> WS.ClientApp ()
loginTest aVer conn = do
    T.putStrLn "Client Connected successfully"
    tR <- async( parseMessage conn)    
    -- Send a verified user and an unverified user,
    -- the recovery should not be showing the unverified user.    
    WS.sendTextData conn $ createLoginRequest testEmail (encode(toJSON $ testLogin)) 
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testLogin
    wait tR

categoryTest :: String -> WS.ClientApp () 
categoryTest aString conn = 
    do
    T.putStrLn "Connected successfully"
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createCategoryRequest testEmail(encode (toJSON (Co.Category aString)))
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testEmail
    wait tR

        
databaseTest :: String -> WS.ClientApp ()
databaseTest aString conn =
    do
    tR <- async $ parseMessage conn
    WS.sendTextData conn $ createQueryDatabaseRequest testEmail $ encode . toJSON $ aString
    WS.sendTextData conn $ createCloseConnection testEmail $ encode $ toJSON testEmail
    wait tR

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
        return $ Co.Party name addresses mapLocation poc primaryCategory vcard 
            alternateCategories alternatePocs
 
-- How do we enforce the rate and factor relationship?  
 
instance Arbitrary Pr.UOM where
    arbitrary = do
        name <- arbitrary
        symbol <- arbitrary
        category <- arbitrary 
        num <- arbitrary
        denom <- arbitrary
        rounding <- arbitrary
        displayDigits <- arbitrary
        uActive <- arbitrary
        return (Pr.createUOM name symbol category num denom displayDigits rounding uActive)

instance Arbitrary Cu.Currency where
      arbitrary = elements [
            ( Cu.Currency "AUD"), 
            ( Cu.Currency "USD"), 
            ( Cu.Currency "GBP"), 
            ( Cu.Currency "ROU"), 
            (Cu.Currency "TST")]

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
        alternateCurrencies <- (orderedList)
        productSet <- orderedList
        return (Co.createCompany party currency (S.fromList alternateCurrencies) (S.fromList productSet))
main = do
    T.putStrLn "Starting server"
    T.putStrLn $ "Removing acid state directory, from previous runs."
    SD.removeDirectoryRecursive acidStateTestDir
    m <- newEmptyMVar
    s <- async (testServerMain m acidStateTestDir)
    T.putStrLn "Waiting for the server to start"
    T.putStrLn "Starting client thread"
    mvarValue <- takeMVar m
    T.putStrLn $ T.pack("Mvar returned " ++ show mvarValue)
    c <- async (WS.runClient "localhost" 8082 "/" $ loginTest 2)
    cat <- async(WS.runClient "localhost" 8082 "/" $ categoryTest "Test Category")
    db <- async (WS.runClient "localhost" 8082 "/" $ databaseTest "Test query database")
    rc <- wait c
    rCat <- wait cat
    rdb <- wait db
    T.putStrLn "End tests"
    -- Cancel the server thread when all tests are done
    cancel s
    mapM_ (\(s, a) -> printf "%-25s" s >> a) tests
    where
        acidStateTestDir = "./dist/build/tests/state"

prop1 aUOM = Pr.validUOM aUOM   
tests = [("properties_tests" :: String, quickCheck prop1),
         ("currency_valid" :: String, quickCheck prop_currency)]

prop_currency aCompany = Co.validCurrencies aCompany