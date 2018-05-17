{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Unsafe arbitrary instances for crypto primitives.

module Test.Pos.Crypto.Arbitrary.Unsafe () where

import           Universum

import           Test.QuickCheck (Arbitrary (..), choose)
import           Test.QuickCheck.Instances ()

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Pos.Binary.Crypto ()
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Crypto.Hashing (AbstractHash, HashAlgorithm, unsafeAbstractHash)
import           Pos.Crypto.SecretSharing (VssKeyPair, VssPublicKey, deterministicVssKeyGen,
                                           toVssPublicKey)

import           Pos.Crypto.Signing (PublicKey, SecretKey, SignTag, Signed, mkSigned)

import           Test.Pos.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..), arbitrarySizedS)

instance ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 64

instance ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Bi.unsafeDeserialize' . Bi.serialize' <$> arbitrarySizedS 128

-- Generating invalid `Signed` objects doesn't make sense even in
-- benchmarks
instance (HasProtocolMagic, Bi a, ArbitraryUnsafe a, Arbitrary SignTag) =>
         ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> pure protocolMagic
                               <*> arbitrary
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
