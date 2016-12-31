{-# LANGUAGE UndecidableInstances #-}

-- | Unsafe arbitrary instances for crypto primitives.

module Pos.Crypto.Arbitrary.Unsafe () where

import           Test.QuickCheck           (Arbitrary (..))
import           Test.QuickCheck.Instances ()
import           Universum

import           Pos.Binary.Class          (Bi)
import qualified Pos.Binary.Class          as Bi
import           Pos.Crypto.SecretSharing  (VssKeyPair, VssPublicKey,
                                            deterministicVssKeyGen, toVssPublicKey)
import           Pos.Crypto.Signing        (PublicKey, SecretKey, Signature, Signed,
                                            mkSigned)
import           Pos.Util.Arbitrary        (ArbitraryUnsafe (..), arbitrarySizedSL)

instance Bi PublicKey => ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Bi.decode <$> arbitrarySizedSL 32

instance Bi SecretKey => ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Bi.decode <$> arbitrarySizedSL 64

instance Bi (Signature a) => ArbitraryUnsafe (Signature a) where
    arbitraryUnsafe = Bi.decode <$> arbitrarySizedSL 64

-- Generating invalid `Signed` objects doesn't make sense even in
-- benchmarks
instance (Bi a, Bi SecretKey, ArbitraryUnsafe a) =>
         ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> arbitraryUnsafe <*> arbitraryUnsafe

-- Again, no sense in generating invalid data, but in benchmarks we
-- don't need Really Secure™ randomness
instance ArbitraryUnsafe VssKeyPair where
    arbitraryUnsafe = deterministicVssKeyGen <$> arbitrary

-- Unfortunately (or fortunately?), we cannot make `VssPublicKey` from
-- random `ByteString`, because its underlying `Bi` instance
-- requires `ByteString` to be a valid representation of a point on a
-- elliptic curve. So we'll stick with taking key out of the valid
-- keypair.
instance ArbitraryUnsafe VssPublicKey where
    arbitraryUnsafe = toVssPublicKey <$> arbitraryUnsafe
