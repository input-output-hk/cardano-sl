module Pos.Crypto.Random
       ( SecureRandom(..)
       , secureRandomBS
       ) where

import           Crypto.Random  (MonadRandom, getRandomBytes)
import qualified Data.ByteArray as ByteArray (convert)
import           OpenSSL.Random (randBytes)
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
