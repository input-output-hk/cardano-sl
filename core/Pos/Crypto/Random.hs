-- | Secure random utilities for cryptographic part.

module Pos.Crypto.Random
       ( SecureRandom(..)
       , secureRandomBS

       , GlobalRandom(..)
       , GlobalRandomGen(..)
       , setGlobalRandom
       , setGlobalRandomSeed

       , deterministic
       , randomNumber
       , randomNumberInRange
       ) where

import           Crypto.Number.Basic     (numBytes)
import           Crypto.Number.Serialize (os2ip)
import           Crypto.OpenSSL.Random   (randBytes)
import           Crypto.Random           (ChaChaDRG, MonadPseudoRandom, MonadRandom,
                                          drgNewSeed, getRandomBytes, seedFromInteger,
                                          withDRG)
import qualified Data.ByteArray          as ByteArray (convert)
import           System.IO.Unsafe        (unsafePerformIO)
import           Universum

-- | Generate a cryptographically random 'ByteString' of specific length.
secureRandomBS :: MonadIO m => Int -> m ByteString
secureRandomBS = liftIO . randBytes

-- | You can use 'runSecureRandom' on any 'MonadRandom' computation to make
-- it use a Really Secure™ randomness source (that is, OpenSSL).
newtype SecureRandom a = SecureRandom {runSecureRandom :: IO a}
    deriving (Functor, Applicative, Monad)

instance MonadRandom SecureRandom where
    getRandomBytes n = SecureRandom (ByteArray.convert <$> secureRandomBS n)

-- | A random generator with global state. Unlike 'SecureRandom', you can
-- set the seed that will be used for 'GlobalRandom', or let it use OpenSSL.
newtype GlobalRandom a = GlobalRandom {runGlobalRandom :: IO a}
    deriving (Functor, Applicative, Monad)

data GlobalRandomGen
    = OpenSSLGen              -- ^ Use OpenSSL for randomness generation
    | ChaChaGen ChaChaDRG     -- ^ Use ChaCha

globalRandomState :: IORef GlobalRandomGen
globalRandomState = unsafePerformIO $ newIORef OpenSSLGen
{-# NOINLINE globalRandomState #-}

setGlobalRandom :: GlobalRandomGen -> IO ()
setGlobalRandom = writeIORef globalRandomState

setGlobalRandomSeed :: Integer -> IO ()
setGlobalRandomSeed =
    setGlobalRandom . ChaChaGen . drgNewSeed . seedFromInteger

instance MonadRandom GlobalRandom where
    getRandomBytes n = GlobalRandom $
        join $ atomicModifyIORef' globalRandomState $ \case
            OpenSSLGen ->
                (OpenSSLGen, ByteArray.convert <$> secureRandomBS n)
            ChaChaGen gen ->
                let (a, gen') = withDRG gen (getRandomBytes n)
                in (ChaChaGen gen', pure a)

-- | You can use 'deterministic' on any 'MonadRandom' computation to make it
-- use a seed (hopefully produced by a Really Secure™ randomness source). The
-- seed has to have enough entropy to make this function secure.
deterministic :: ByteString -> MonadPseudoRandom ChaChaDRG a -> a
deterministic seed gen = fst $ withDRG chachaSeed gen
  where
    chachaSeed = drgNewSeed . seedFromInteger . os2ip $ seed

-- | Generate a random number in range [0, n).
--
-- We want to avoid modulo bias, so we use the arc4random_uniform
-- implementation (http://stackoverflow.com/a/20051580/615030). Specifically,
-- we repeatedly generate a random number in range [0, 2^x) until we hit on
-- something outside of [0, 2^x mod n), which means that it'll be in range
-- [2^x mod n, 2^x). The amount of numbers in this interval is guaranteed to
-- be divisible by n, and thus applying 'mod' to it will be safe.
randomNumber :: MonadRandom m => Integer -> m Integer
randomNumber n
    | n <= 0 = error "randomNumber: n <= 0"
    | otherwise = gen
  where
    size = max 4 (numBytes n)             -- size of integers, in bytes
    rangeMod = 2 ^ (size * 8) `rem` n     -- 2^x mod n
    gen = do
        x <- os2ip @ByteString <$> getRandomBytes size
        if x < rangeMod then gen else return (x `rem` n)

randomNumberInRange :: MonadRandom m => Integer -> Integer -> m Integer
randomNumberInRange a b
    | a > b     = error "randomNumberInRange: a > b"
    | otherwise = (a +) <$> randomNumber (b - a + 1)
