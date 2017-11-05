-- | Unsafe arbitrary instances for crypto primitives.

module Pos.Arbitrary.Crypto.Unsafe () where

import           Universum

import           Test.QuickCheck                 (Arbitrary (..), choose)
import           Test.QuickCheck.Instances       ()

import           Pos.Binary.Class                (Bi)
import qualified Pos.Binary.Class                as Bi
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Crypto.Hashing              (AbstractHash, HashAlgorithm,
                                                  unsafeAbstractHash)
import           Pos.Crypto.SecretSharing        (VssKeyPair, VssPublicKey,
                                                  deterministicVssKeyGen, toVssPublicKey)
import           Pos.Crypto.Signing              (PublicKey, SecretKey, Signed, mkSigned)
import           Pos.Crypto.Signing.Types.Tag    (SignTag)
import           Pos.Util.Arbitrary              (ArbitraryUnsafe (..), arbitrarySizedSL)


instance Bi PublicKey => ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Bi.deserializeThrow . Bi.serialize <$> arbitrarySizedSL 64

instance Bi SecretKey => ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Bi.deserializeThrow . Bi.serialize <$> arbitrarySizedSL 128

-- Generating invalid `Signed` objects doesn't make sense even in
-- benchmarks
instance (HasProtocolConstants, Bi a, Bi SecretKey, ArbitraryUnsafe a, Arbitrary SignTag) =>
         ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> arbitrary
                               <*> arbitraryUnsafe
                               <*> arbitraryUnsafe

instance ArbitraryUnsafe VssKeyPair where
    arbitraryUnsafe = deterministicVssKeyGen <$> arbitrary

-- Unfortunately (or fortunately?), we cannot make `VssPublicKey` from
-- random `ByteString`, because its underlying `Bi` instance
-- requires `ByteString` to be a valid representation of a point on a
-- elliptic curve. So we'll stick with taking key out of the valid
-- keypair.
instance ArbitraryUnsafe VssPublicKey where
    arbitraryUnsafe = toVssPublicKey <$> arbitraryUnsafe

instance (HashAlgorithm algo, Bi a) =>
         ArbitraryUnsafe (AbstractHash algo a) where
    arbitraryUnsafe = unsafeAbstractHash <$>
        choose (minBound, maxBound :: Word64)
