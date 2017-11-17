module MarshallingSpec where

import           Universum

import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Orphans ()
import           Data.Aeson
import           Pos.Util.BackupPhrase (BackupPhrase)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Pos.Core.Types as Core

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
spec :: Spec
spec = describe "Marshalling & Unmarshalling" $ do
  prop "Aeson BackupPhrase roundtrips" (aesonRoundtrip @BackupPhrase Proxy)
  prop "Aeson AssuranceLevel roundtrips" (aesonRoundtrip @AssuranceLevel Proxy)
  prop "Aeson Coin roundtrips" (aesonRoundtrip @Core.Coin Proxy)


aesonRoundtrip :: (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => proxy a -> Property
aesonRoundtrip (_ :: proxy a) = forAll arbitrary $ \(s :: a) -> do
    decode (encode (toJSON s)) === Just s
