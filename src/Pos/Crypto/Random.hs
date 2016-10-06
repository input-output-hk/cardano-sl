{-# LANGUAGE TypeApplications #-}

module Pos.Crypto.Random
       ( SecureRandom(..)
       , secureRandomBS
       , secureRandomNumber
       ) where

import           Crypto.Number.Basic     (numBytes)
import           Crypto.Number.Serialize (os2ip)
import           Crypto.Random           (MonadRandom, drgNewTest, getRandomBytes,
                                          withDRG)
import qualified Data.Binary             as Binary
import qualified Data.ByteArray          as ByteArray (convert)
import qualified Data.ByteString         as BS
import           OpenSSL.Random          (randBytes)
import           Universum

-- | Generate a cryptographically random 'ByteString' of specific length.
secureRandomBS :: Int -> IO ByteString
secureRandomBS = randBytes

-- | You can use 'runSecureRandom' on any 'MonadRandom' computation to make
-- it use a Really Secure™ randomness source (that is, OpenSSL).
newtype SecureRandom a = SecureRandom {runSecureRandom :: IO a}
    deriving (Functor, Applicative, Monad)

instance MonadRandom SecureRandom where
    getRandomBytes n = SecureRandom (ByteArray.convert <$> secureRandomBS n)

-- | Given a 40-byte seed, generate a random number in range [0, n).
--
-- We want to avoid modulo bias, so we use the arc4random_uniform
-- implementation (http://stackoverflow.com/a/20051580/615030). Specifically,
-- we repeatedly generate a random number in range [0, 2^x) until we hit on
-- something outside of [0, 2^x mod n), which means that it'll be in range
-- [2^x mod n, 2^x). The amount of numbers in this interval is guaranteed to
-- be divisible by n, and thus applying 'mod' to it will be safe.
secureRandomNumber :: ByteString -> Integer -> Integer
secureRandomNumber seed n
    | n <= 0 = panic "secureRandomNumber: n<=0"
    | BS.length seed /= 40 = panic "secureRandomNumber: length seed /= 40"
    | otherwise = fst $ withDRG (drgNewTest chachaSeed) gen
  where
    chachaSeed = Binary.decode (toS seed)
    size = max 4 (numBytes n)         -- size of integers, in bytes
    rangeMod = 2^(size*8) `rem` n     -- 2^x mod n
    gen = do
        x <- os2ip @ByteString <$> getRandomBytes size
        if x < rangeMod then gen else return (x `rem` n)
