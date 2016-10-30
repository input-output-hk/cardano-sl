{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Crypto.Arbitrary.Unsafe () where

import           Data.Binary              (Binary)
import qualified Data.Binary              as Binary
import           Test.QuickCheck          (Arbitrary (..))
import           Universum

import           Pos.Crypto.SecretSharing (VssKeyPair, VssPublicKey,
                                           deterministicVssKeyGen)
import           Pos.Crypto.Signing       (PublicKey, SecretKey, Signature, Signed,
                                           mkSigned)
import           Pos.Util.Arbitrary       (ArbitraryUnsafe (..), arbitrarySizedSL)

instance ArbitraryUnsafe PublicKey where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 32

instance ArbitraryUnsafe SecretKey where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 64

instance ArbitraryUnsafe (Signature a) where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 64

-- Generating invalid `Signed` objects doesn't make sense even in benchmarks
instance (Binary a, ArbitraryUnsafe a) => ArbitraryUnsafe (Signed a) where
    arbitraryUnsafe = mkSigned <$> arbitraryUnsafe <*> arbitraryUnsafe

-- TODO: It *seems* to be that PVSS public key is indeed 32 bytes long,
-- but it's better to check
instance ArbitraryUnsafe VssPublicKey where
    arbitraryUnsafe = Binary.decode <$> arbitrarySizedSL 32

-- Again, no sense in generating invalid data, but in benchmarks
-- we don't need Really Secure™ randomness
instance ArbitraryUnsafe VssKeyPair where
    arbitraryUnsafe = deterministicVssKeyGen <$> arbitrary
