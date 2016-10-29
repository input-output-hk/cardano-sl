{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Crypto.Arbitrary.Unsafe () where

import           Data.Binary              (Binary)
import qualified Data.Binary              as Binary
import           Test.QuickCheck          (Arbitrary (..))
import           Universum

import           Pos.Crypto.SecretSharing (VssKeyPair, VssPublicKey,
                                           deterministicVssKeyGen,
                                           toVssPublicKey)
import           Pos.Crypto.Signing       (PublicKey, SecretKey, Signature,
                                           Signed, mkSigned)
import           Pos.Util.Arbitrary       (ArbitraryUnsafe (..),
                                           arbitrarySizedSL)

instance ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 32

instance ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 64

instance ArbitraryUnsafe (Signature a) where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 64

-- Generating invalid `Signed` objects doesn't make sense even in benchmarks
instance (Binary a, ArbitraryUnsafe a) => ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> arbitraryUnsafe <*> arbitraryUnsafe

-- Again, no sense in generating invalid data, but in benchmarks
-- we don't need Really Secure™ randomness
instance ArbitraryUnsafe VssKeyPair where
    arbitraryUnsafe = deterministicVssKeyGen <$> arbitrary

-- Unfortunately (or fortunately?), we cannot make `VssPublicKey` from random `ByteString`,
-- because its underlying `Binary` instance requires `ByteString` to be a valid representation
-- of a point on a elliptic curve. So we'll stick with taking key out of the valid keypair.
instance ArbitraryUnsafe VssPublicKey where
    arbitraryUnsafe = toVssPublicKey <$> arbitraryUnsafe
