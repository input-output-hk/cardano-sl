module MarshallingSpec where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError)
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans ()
import           Data.Aeson
import           Data.Typeable (typeRep)
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Pos.Core as Core

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: Spec
spec = describe "Marshalling & Unmarshalling" $ do
  aesonRoundtripProp @BackupPhrase Proxy
  aesonRoundtripProp @AssuranceLevel Proxy
  aesonRoundtripProp @Payment Proxy
  aesonRoundtripProp @PaymentDistribution Proxy
  aesonRoundtripProp @NewWallet Proxy
  aesonRoundtripProp @Core.Coin Proxy
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


aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    decode (encode (toJSON s)) === Just s

aesonRoundtripProp
    :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => proxy a -> Spec
aesonRoundtripProp proxy =
    prop ("Aeson " <> show (typeRep proxy) <> " roundtrips") (aesonRoundtrip proxy)
