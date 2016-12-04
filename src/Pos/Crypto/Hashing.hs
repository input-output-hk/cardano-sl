{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | Hashing capabilities.

module Pos.Crypto.Hashing
       ( WithHash (whData, whHash)
       , withHash

       , AbstractHash (..)
       , Hash
       , hashHexF
       , shortHashF
       , hash
       , hashRaw
       , unsafeHash

       , CastHash (castHash)
       ) where

import           Control.DeepSeq     (force)
import           Control.Monad.Fail  (fail)
import           Crypto.Hash         (Blake2b_512, Digest, HashAlgorithm,
                                      digestFromByteString)
import qualified Crypto.Hash         as Hash (hash, hashDigestSize, hashlazy)
import           Data.Aeson          (ToJSON (toJSON))
import           Data.Binary         (Binary (..))
import qualified Data.Binary         as Binary
import qualified Data.Binary.Get     as Binary (getByteString)
import qualified Data.Binary.Put     as Binary (putByteString)
import qualified Data.ByteArray      as ByteArray
import           Data.Hashable       (Hashable (hashWithSalt), hashPtrWithSalt)
import           Data.SafeCopy       (SafeCopy (..))
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, fitLeft, later, shown, (%.))
import           System.IO.Unsafe    (unsafeDupablePerformIO)
import           Universum

import           Pos.Util            (Raw, getCopyBinary, putCopyBinary)

----------------------------------------------------------------------------
-- WithHash
----------------------------------------------------------------------------
data WithHash a = WithHash
    { whData :: a
    , whHash :: Hash a
    } deriving (Show)

instance Binary a => Binary (WithHash a) where
    put = put. whData
    get = withHash <$> get

instance Binary a => SafeCopy (WithHash a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "WithHash"

instance Hashable (WithHash a) where
    hashWithSalt s = hashWithSalt s . whHash

instance Buildable.Buildable a => Buildable.Buildable (WithHash a) where
    build = Buildable.build . whData

instance Eq a => Eq (WithHash a) where
    a == b = (whHash a == whHash b) && (whData a == whData b)

instance Ord a => Ord (WithHash a) where
    a <= b = whData a <= whData b

withHash :: Binary a => a -> WithHash a
withHash a = WithHash a (force h)
  where
    h = hash a

-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2b_512

instance Hashable (AbstractHash algo a) where
    hashWithSalt s h = unsafeDupablePerformIO $ ByteArray.withByteArray h (\ptr -> hashPtrWithSalt ptr len s)
      where
        !len = ByteArray.length h

instance HashAlgorithm algo => SafeCopy (AbstractHash algo a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "AbstractHash"

instance HashAlgorithm algo => Binary (AbstractHash algo a) where
    {-# SPECIALIZE instance Binary (Hash a) #-}
    get = do
        bs <- Binary.getByteString $ Hash.hashDigestSize @algo $
              panic "Pos.Crypto.Hashing.get: HashAlgorithm value is evaluated!"
        case digestFromByteString bs of
            -- It's impossible because getByteString will already fail if
            -- there weren't enough bytes available
            Nothing -> fail "Pos.Crypto.Hashing.get: impossible"
            Just x  -> return (AbstractHash x)
    put (AbstractHash h) =
        Binary.putByteString (ByteArray.convert h)

instance Buildable.Buildable (AbstractHash algo a) where
    build (AbstractHash x) = bprint shown x

instance ToJSON (Hash a) where
    toJSON = toJSON . pretty

-- | Short version of 'unsafeHash'.
hash :: Binary a => a -> Hash a
hash = unsafeHash

-- | Raw constructor application.
hashRaw :: ByteString -> Hash Raw
hashRaw = AbstractHash . Hash.hash

-- | Encode thing as 'Binary' data and then wrap into constructor.
unsafeHash :: Binary a => a -> Hash b
unsafeHash = AbstractHash . Hash.hashlazy . Binary.encode

-- | Specialized formatter for 'Hash'.
hashHexF :: Format r (Hash a -> r)
hashHexF = later $ \(AbstractHash x) -> Buildable.build (show x :: Text)

-- | Smart formatter for 'Hash' to show only first @8@ characters of 'Hash'.
shortHashF :: Format r (Hash a -> r)
shortHashF = fitLeft 8 %. hashHexF

-- | Type class for unsafe cast between hashes.
-- You must ensure that types have identical Binary instances.
class CastHash a b where
    castHash :: AbstractHash algo a -> AbstractHash algo b
    castHash (AbstractHash x) = AbstractHash x

instance CastHash a a where
    castHash = identity
