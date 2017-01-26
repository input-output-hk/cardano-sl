{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Hashing capabilities.

module Pos.Crypto.Hashing
       (
         -- * WithHash
         WithHash (..)
       , withHash
       , _whData
       , _whHash

         -- * AbstractHash
       , AbstractHash (..)
       , encodeAbstractHash
       , decodeAbstractHash
       , encodeHash
       , decodeHash
       , abstractHash
       , unsafeAbstractHash

         -- * Common Hash
       , Hash
       , hashHexF
       , shortHashF
       , hash
       , hashRaw
       , unsafeHash

         -- * Utility
       , CastHash (castHash)
       , HashAlgorithm
       ) where

import           Control.DeepSeq      (force)
import           Control.Lens         (makeLensesFor)
import           Crypto.Hash          (Blake2s_224, Digest, HashAlgorithm)
import qualified Crypto.Hash          as Hash (hash, hashlazy)
import qualified Data.ByteArray       as ByteArray
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable        (Hashable (hashWithSalt), hashPtrWithSalt)
import           Data.SafeCopy        (SafeCopy (..))
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (Format, bprint, fitLeft, later, (%.))
import qualified Serokell.Util.Base64 as B64
import           System.IO.Unsafe     (unsafeDupablePerformIO)
import           Universum

import           Pos.Binary.Class     (Bi)
import qualified Pos.Binary.Class     as Bi
import           Pos.Util             (Raw, getCopyBinary, putCopyBinary)

----------------------------------------------------------------------------
-- WithHash
----------------------------------------------------------------------------
data WithHash a = WithHash
    { whData :: a
    , whHash :: Hash a
    } deriving (Show, Typeable)

instance Hashable (WithHash a) where
    hashWithSalt s = hashWithSalt s . whHash

instance Buildable.Buildable a => Buildable.Buildable (WithHash a) where
    build = Buildable.build . whData

instance Eq a => Eq (WithHash a) where
    a == b = (whHash a == whHash b) && (whData a == whData b)

instance Ord a => Ord (WithHash a) where
    a <= b = whData a <= whData b

withHash :: Bi a => a -> WithHash a
withHash a = WithHash a (force h)
  where
    h = hash a

-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

instance Hashable (AbstractHash algo a) where
    hashWithSalt s h =
        unsafeDupablePerformIO $
        ByteArray.withByteArray h (\ptr -> hashPtrWithSalt ptr len s)
      where
        !len = ByteArray.length h

instance Bi (AbstractHash algo a) =>
         SafeCopy (AbstractHash algo a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "AbstractHash"

instance Buildable.Buildable (AbstractHash algo a) where
    build = bprint shortHashF

-- | Encode hash from base64 form.
encodeAbstractHash :: forall algo a . Bi (AbstractHash algo a) => AbstractHash algo a -> Text
encodeAbstractHash = B64.encode . Bi.encodeStrict

-- | Parses given hash in base64 form.
decodeAbstractHash :: forall algo a . Bi (AbstractHash algo a) => Text -> AbstractHash algo a
decodeAbstractHash = Bi.decode . processRes . B64.decode
  where
    processRes (Right x) = BSL.fromStrict x
    processRes (Left e) = panic $ "decode hash error: " <> e

-- | Encode hash from base64 form.
encodeHash :: Bi (Hash a) => Hash a -> Text
encodeHash = encodeAbstractHash @Blake2s_224

-- | Parses given hash in base64 form.
decodeHash :: Bi (Hash a) => Text -> Hash a
decodeHash = decodeAbstractHash @Blake2s_224

-- | Encode thing as 'Binary' data and then wrap into constructor.
abstractHash
    :: (HashAlgorithm algo, Bi a)
    => a -> AbstractHash algo a
abstractHash = unsafeAbstractHash

-- | Unsafe version of abstractHash.
unsafeAbstractHash
    :: (HashAlgorithm algo, Bi a)
    => a -> AbstractHash algo b
unsafeAbstractHash = AbstractHash . Hash.hashlazy . Bi.encode

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2s_224

-- | Short version of 'unsafeHash'.
hash :: Bi a => a -> Hash a
hash = unsafeHash

-- | Raw constructor application.
hashRaw :: ByteString -> Hash Raw
hashRaw = AbstractHash . Hash.hash

-- | Encode thing as 'Bi' data and then wrap into constructor.
unsafeHash :: Bi a => a -> Hash b
unsafeHash = unsafeAbstractHash

-- | Specialized formatter for 'Hash'.
hashHexF :: Format r (AbstractHash algo a -> r)
hashHexF = later $ \(AbstractHash x) -> Buildable.build (show x :: Text)

-- | Smart formatter for 'Hash' to show only first @8@ characters of 'Hash'.
shortHashF :: Format r (AbstractHash algo a -> r)
shortHashF = fitLeft 8 %. hashHexF


-- | Type class for unsafe cast between hashes.
-- You must ensure that types have identical Bi instances.
class CastHash a b where
    castHash :: AbstractHash algo a -> AbstractHash algo b
    castHash (AbstractHash x) = AbstractHash x

instance CastHash a a where
    castHash = identity

-- | Lenses for 'WithHash'
makeLensesFor [("whData", "_whData"), ("whHash", "_whHash")] ''WithHash
