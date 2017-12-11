module MarshallingSpec where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError)
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans ()
import           Data.Aeson
import           Data.Typeable (typeRep)
import           Pos.Crypto (PassPhrase, emptyPassphrase)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Pos.Util.BackupPhrase (BackupPhrase)

import qualified Pos.Core as Core

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: Spec
spec = describe "Marshalling & Unmarshalling" $ do
    describe "Roundtrips" $ do
        aesonRoundtripProp @BackupPhrase Proxy
        aesonRoundtripProp @Account Proxy
        aesonRoundtripProp @AssuranceLevel Proxy
        aesonRoundtripProp @Payment Proxy
        aesonRoundtripProp @PaymentDistribution Proxy
        aesonRoundtripProp @NewWallet Proxy
        aesonRoundtripProp @Core.Coin Proxy
        aesonRoundtripProp @PassPhrase Proxy
        aesonRoundtripProp @TransactionGroupingPolicy Proxy
        aesonRoundtripProp @TransactionType Proxy
        aesonRoundtripProp @TransactionDirection Proxy
        aesonRoundtripProp @Transaction Proxy
        aesonRoundtripProp @WalletError Proxy
        aesonRoundtripProp @SlotDuration Proxy
        aesonRoundtripProp @LocalTimeDifference Proxy
        aesonRoundtripProp @BlockchainHeight Proxy
        aesonRoundtripProp @SyncProgress Proxy
        aesonRoundtripProp @NodeInfo Proxy
        aesonRoundtripProp @NodeSettings Proxy

    describe "Invariants" $ do
        describe "password" $ do
            it "empty string decodes to empty password" $
                jsonString "" `decodesTo` (== emptyPassphrase)
            it "base-16 string of length 32 decodes to nonempty password" $
                jsonString (fromString $ replicate 64 'a')
                    `decodesTo` (/= emptyPassphrase)
            it "invalid length password decoding fails" $
                -- currently passphrase should be either empty or of length 32
                decodingFails @PassPhrase "aabbcc" Proxy


aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    eitherDecode (encode (toJSON s)) === Right s

aesonRoundtripProp
    :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => proxy a -> Spec
aesonRoundtripProp proxy =
    prop ("Aeson " <> show (typeRep proxy) <> " roundtrips") (aesonRoundtrip proxy)

decodesTo :: (FromJSON a, Show a) => LByteString -> (a -> Bool) -> Expectation
decodesTo s p = either expectationFailure (`shouldSatisfy` p) $ eitherDecode s

decodingFails :: (FromJSON a, Show a) => LByteString -> proxy a -> Expectation
decodingFails s (_ :: proxy a) = eitherDecode @a s `shouldSatisfy` isLeft

-- | Take a string value, and make a JSON-string from it.
jsonString :: LByteString -> LByteString
jsonString bs = "\"" <> bs <> "\""
