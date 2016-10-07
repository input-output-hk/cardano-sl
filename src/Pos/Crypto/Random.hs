{-# LANGUAGE TypeApplications #-}

module Pos.Crypto.Random
       ( SecureRandom(..)
       , secureRandomBS

       , deterministic
       , randomNumber
       ) where

import           Crypto.Number.Basic     (numBytes)
import           Crypto.Number.Serialize (os2ip)
import           Crypto.Random           (ChaChaDRG, MonadPseudoRandom, MonadRandom,
                                          drgNewTest, getRandomBytes, withDRG)
import qualified Data.Binary             as Binary
import qualified Data.ByteArray          as ByteArray (convert)
import qualified Data.ByteString         as BS
import           OpenSSL.Random          (randBytes)
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

-- | You can use 'deterministic' on any 'MonadRandom' computation to make it
-- use a seed (hopefully produced by a Really Secure™ randomness source). The
-- seed has to be exactly 40 bytes long.
deterministic :: ByteString -> MonadPseudoRandom ChaChaDRG a -> a
deterministic seed gen
    | BS.length seed /= 40 = panic "deterministic: length seed /= 40"
    | otherwise = fst $ withDRG (drgNewTest chachaSeed) gen
  where
    chachaSeed = Binary.decode (toS seed)

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
    | n <= 0 = panic "randomNumber: n <= 0"
    | otherwise = gen
  where
    size = max 4 (numBytes n)             -- size of integers, in bytes
    rangeMod = 2 ^ (size * 8) `rem` n     -- 2^x mod n
    gen = do
        x <- os2ip @ByteString <$> getRandomBytes size
        if x < rangeMod then gen else return (x `rem` n)
