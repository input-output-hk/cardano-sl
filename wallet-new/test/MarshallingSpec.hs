module MarshallingSpec where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError)
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans ()
import           Data.Aeson
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Pos.Core as Core

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: Spec
spec = describe "Marshalling & Unmarshalling" $ do
    prop "Aeson BackupPhrase roundtrips" (aesonRoundtrip @BackupPhrase Proxy)
    prop "Aeson AssuranceLevel roundtrips" (aesonRoundtrip @AssuranceLevel Proxy)
    prop "Aeson Payment roundtrips" (aesonRoundtrip @Payment Proxy)
    prop "Aeson PaymentDistribution roundtrips" (aesonRoundtrip @PaymentDistribution Proxy)
    prop "Aeson NewWallet roundtrips" (aesonRoundtrip @NewWallet Proxy)
    prop "Aeson Coin roundtrips" (aesonRoundtrip @Core.Coin Proxy)
    prop "Aeson TransactionGroupingPolicy roundtrips" (aesonRoundtrip @TransactionGroupingPolicy Proxy)
    prop "Aeson TransactionType roundtrips" (aesonRoundtrip @TransactionType Proxy)
    prop "Aeson TransactionDirection roundtrips" (aesonRoundtrip @TransactionDirection Proxy)
    prop "Aeson Transaction roundtrips" (aesonRoundtrip @Transaction Proxy)
    prop "Aeson WalletError roundtrips" (aesonRoundtrip @WalletError Proxy)
    prop "Aeson SlotDuration roundtrips" (aesonRoundtrip @SlotDuration Proxy)
    prop "Aeson LocalTimeDifference roundtrips" (aesonRoundtrip @LocalTimeDifference Proxy)
    prop "Aeson BlockchainHeight roundtrips" (aesonRoundtrip @BlockchainHeight Proxy)
    prop "Aeson SyncProgress roundtrips" (aesonRoundtrip @SyncProgress Proxy)
    prop "Aeson NodeInfo roundtrips" (aesonRoundtrip @NodeInfo Proxy)
    prop "Aeson NodeSettings roundtrips" (aesonRoundtrip @NodeSettings Proxy)

aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    decode (encode (toJSON s)) === Just s
