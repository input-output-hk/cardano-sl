module MarshallingSpec (spec) where

import           Universum

import           Control.Lens (from, to)
import           Data.Aeson
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Typeable (typeRep)
import           Pos.Client.Txp.Util (InputSelectionPolicy)
import qualified Pos.Crypto as Crypto
import qualified Pos.Txp.Toil.Types as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Property as Property

import           Pos.Util.BackupPhrase (BackupPhrase)

import qualified Pos.Core as Core

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.V1.Errors (WalletError)
import           Cardano.Wallet.API.V1.Migration.Types (Migrate (..))
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans ()
import qualified Cardano.Wallet.Util as Util

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: Spec
spec = parallel $ describe "Marshalling & Unmarshalling" $ do
    parallel $ describe "Roundtrips" $ do
        -- Aeson roundrips
        aesonRoundtripProp @(V1 BackupPhrase) Proxy
        aesonRoundtripProp @Account Proxy
        aesonRoundtripProp @AssuranceLevel Proxy
        aesonRoundtripProp @(V1 Core.SoftwareVersion) Proxy
        aesonRoundtripProp @NodeSettings Proxy
        aesonRoundtripProp @Payment Proxy
        aesonRoundtripProp @PaymentDistribution Proxy
        aesonRoundtripProp @NewWallet Proxy
        aesonRoundtripProp @NewAddress Proxy
        aesonRoundtripProp @(V1 Core.Coin) Proxy
        aesonRoundtripProp @(V1 Crypto.PassPhrase) Proxy
        aesonRoundtripProp @(V1 InputSelectionPolicy) Proxy
        aesonRoundtripProp @TimeInfo Proxy
        aesonRoundtripProp @Transaction Proxy
        aesonRoundtripProp @(V1 Core.Timestamp) Proxy
        aesonRoundtripProp @TransactionDirection Proxy
        aesonRoundtripProp @TransactionType Proxy
        aesonRoundtripProp @TransactionStatus Proxy
        aesonRoundtripProp @WalletError Proxy
        aesonRoundtripProp @WalletId Proxy
        aesonRoundtripProp @Wallet Proxy
        aesonRoundtripProp @SlotDuration Proxy
        aesonRoundtripProp @LocalTimeDifference Proxy
        aesonRoundtripProp @BlockchainHeight Proxy
        aesonRoundtripProp @SyncPercentage Proxy
        aesonRoundtripProp @NodeInfo Proxy
        aesonRoundtripProp @SyncState Proxy
        aesonRoundtripProp @EstimatedCompletionTime Proxy
        aesonRoundtripProp @SyncProgress Proxy
        aesonRoundtripProp @SyncThroughput Proxy

        -- Migrate roundrips
        migrateRoundtripProp @(V1 Core.Address) @(V0.CId V0.Addr) Proxy Proxy
        migrateRoundtripProp @(V1 Core.Coin) @V0.CCoin Proxy Proxy
        migrateRoundtripProp @AssuranceLevel @V0.CWalletAssurance Proxy Proxy
        migrateRoundtripProp @WalletId @(V0.CId V0.Wal) Proxy Proxy
        migrateRoundtripProp @(WalletId, AccountIndex) @V0.AccountId Proxy Proxy
        migrateRoundtripProp @PaymentDistribution @(V0.CId V0.Addr, Core.Coin) Proxy Proxy
        migrateRoundtripProp @EstimatedFees @V0.TxFee Proxy Proxy

        -- Other roundtrips
        generalRoundtripProp "UTC time" Util.showApiUtcTime Util.parseApiUtcTime

    describe "Invariants" $ do
        describe "password" $ do
            it "empty string decodes to empty password" $
                jsonString "" `decodesTo` (== V1 (Crypto.emptyPassphrase))
            it "base-16 string of length 32 decodes to nonempty password" $
                jsonString (fromString $ replicate 64 'a')
                    `decodesTo` (/= V1 (Crypto.emptyPassphrase))
            it "invalid length password decoding fails" $
                -- currently passphrase should be either empty or of length 32
                decodingFails @(V1 Crypto.PassPhrase) "aabbcc" Proxy

    describe "Timestamp Parsing" $ do
        describe "ToIndex" $ do
            let toIndex' :: Text -> Maybe (V1 Core.Timestamp)
                toIndex' = toIndex (Proxy @Transaction)
            it "can parse an ISO8601 UTC formatted date" $ do
                toIndex' "1999-10-12"
                    `shouldBe`
                        Just (UTCTime (fromGregorian 1999 10 12) 0
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an ISO8601 UTC formatted datetime (seconds)" $ do
                toIndex' "1999-10-12T22:15:31.123"
                    `shouldBe`
                        Just (
                            UTCTime
                                (fromGregorian 1999 10 12)
                                ((22 * 60 * 60) + (15 * 60) + 31.123)
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an ISO8601 UTC formatted datetime (fractional)" $ do
                toIndex' "1999-10-12T22:15:37"
                    `shouldBe`
                        Just (
                            UTCTime
                                (fromGregorian 1999 10 12)
                                ((22 * 60 * 60) + (15 * 60) + 37)
                            ^. from Core.timestampToUTCTimeL . to V1
                            )
            it "can parse an integral timestamp" $ do
                toIndex' "123456789"
                    `shouldBe`
                        Just ((123456789 :: POSIXTime)
                            ^. from Core.timestampSeconds . to V1
                            )
            it "can parse an fractional timestamp" $ do
                toIndex' "123456789.123"
                    `shouldBe`
                        Just ((123456789.123 :: POSIXTime)
                            ^. from Core.timestampSeconds . to V1
                            )


migrateRoundtrip :: (Arbitrary from, Migrate from to, Migrate to from, Eq from, Show from) => proxy from -> proxy to -> Property
migrateRoundtrip (_ :: proxy from) (_ :: proxy to) = forAll arbitrary $ \(arbitraryFrom :: from) -> do
    (eitherMigrate =<< migrateTo arbitraryFrom) === Right arbitraryFrom
  where
    migrateTo x = eitherMigrate x :: Either WalletError to

migrateRoundtripProp
    :: (Arbitrary from, Migrate from to, Migrate to from, Eq from, Show from, Typeable from, Typeable to)
    => proxy from -> proxy to -> Spec
migrateRoundtripProp proxyFrom proxyTo =
    prop ("Migrate " <> show (typeRep proxyFrom) <> " <-> " <> show (typeRep proxyTo) <> " roundtrips") (migrateRoundtrip proxyFrom proxyTo)

aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    eitherDecode (encode s) === Right s

aesonRoundtripProp
    :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => proxy a -> Spec
aesonRoundtripProp proxy =
    prop ("Aeson " <> show (typeRep proxy) <> " roundtrips") (aesonRoundtrip proxy)

generalRoundtrip
    :: (Arbitrary from, Eq from, Show from, Show e)
    => (from -> to) -> (to -> Either e from) -> Property
generalRoundtrip generalEncode generalDecode = property $ \a ->
    case generalDecode (generalEncode a) of
        Right a' -> a === a'
        Left e   -> property Property.failed{ Property.reason = show e }

generalRoundtripProp
    :: (Arbitrary from, Eq from, Show from, Show e)
    => String -> (from -> to) -> (to -> Either e from) -> Spec
generalRoundtripProp desc generalEncode generalDecode =
    prop (desc <> " roundtrip") $ generalRoundtrip generalEncode generalDecode

decodesTo :: (FromJSON a, Show a) => LByteString -> (a -> Bool) -> Expectation
decodesTo s p = either expectationFailure (`shouldSatisfy` p) $ eitherDecode s

decodingFails :: (FromJSON a, Show a) => LByteString -> proxy a -> Expectation
decodingFails s (_ :: proxy a) = eitherDecode @a s `shouldSatisfy` isLeft

-- | Take a string value, and make a JSON-string from it.
jsonString :: LByteString -> LByteString
jsonString bs = "\"" <> bs <> "\""
