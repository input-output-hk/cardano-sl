{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hashing capabilities.

module Pos.Crypto.Hashing
       (
         -- * WithHash
         WithHash (..)
       , withHash
       , withHashCbor
       , _whData
       , _whHash

         -- * AbstractHash
       , AbstractHash (..)
       , decodeAbstractHash
       , decodeHash
       , abstractHash
       , unsafeAbstractHash

         -- * Common Hash
       , Hash
       , hashHexF
       , shortHashF
       , hash
       , hashCbor
       , hashRaw
       , unsafeHash

         -- * Utility
       , CastHash (castHash)
       , HashAlgorithm
       , hashDigestSize'
       , reifyHashDigestSize
       ) where

import           Universum

import           Control.Lens         (makeLensesFor)
import           Crypto.Hash          (Blake2b_256, Digest, HashAlgorithm, hashDigestSize)
import qualified Crypto.Hash          as Hash
import qualified Data.ByteArray       as ByteArray
import           Data.Hashable        (Hashable (hashWithSalt), hashPtrWithSalt)
import           Data.Reflection      (reifyNat)
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (Format, bprint, fitLeft, later, (%.))
import qualified Prelude
import qualified Serokell.Util.Base16 as B16
import           System.IO.Unsafe     (unsafeDupablePerformIO)

import qualified Pos.Binary.Cbor      as Cbor
import           Pos.Binary.Class     (Bi, Raw)
import qualified Pos.Binary.Class     as Bi

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

withHashCbor :: Cbor.Bi a => a -> WithHash a
withHashCbor a = WithHash a (force h)
  where
    h = hashCbor a

-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

instance HashAlgorithm algo => Read (AbstractHash algo a) where
    readsPrec _ s = case B16.decode (toText s) of
        Left _   -> []
        Right bs -> case Hash.digestFromByteString bs of
            Nothing -> []
            Just h  -> [(AbstractHash h, "")]

instance Hashable (AbstractHash algo a) where
    hashWithSalt s h =
        unsafeDupablePerformIO $
        ByteArray.withByteArray h (\ptr -> hashPtrWithSalt ptr len s)
      where
        !len = ByteArray.length h

instance Buildable.Buildable (AbstractHash algo a) where
    build = bprint shortHashF

hashDigestSize' :: forall algo . HashAlgorithm algo => Int
hashDigestSize' = hashDigestSize @algo
    (error "Pos.Crypto.Hashing.hashDigestSize': HashAlgorithm value is evaluated!")

reifyHashDigestSize
    :: forall algo r.
       HashAlgorithm algo
    => (forall n . KnownNat n => Proxy n -> r)
    -> r
reifyHashDigestSize = reifyNat (fromIntegral (hashDigestSize' @algo))

-- | Parses given hash in base16 form.
decodeAbstractHash
    :: forall algo a.
       Bi (AbstractHash algo a)
    => Text -> AbstractHash algo a
decodeAbstractHash = processRes . Bi.decodeFull . processRes . B16.decode
  where
    processRes (Right x) = x
    processRes (Left e)  = error $ "decode hash error: " <> e

-- | Parses given hash in base16 form.
decodeHash :: Bi (Hash a) => Text -> Hash a
decodeHash = decodeAbstractHash @Blake2b_256

-- | Encode thing as 'Binary' data and then wrap into constructor.
abstractHash
    :: (HashAlgorithm algo, Bi a)
    => a -> AbstractHash algo a
abstractHash = unsafeAbstractHash

-- | Unsafe version of abstractHash.
unsafeAbstractHash
    :: (HashAlgorithm algo, Bi a)
    => a -> AbstractHash algo b
unsafeAbstractHash = AbstractHash . Hash.hash . Bi.encode

-- | Unsafe version of abstractHash.
unsafeAbstractHashCbor
    :: (HashAlgorithm algo, Cbor.Bi a)
    => a -> AbstractHash algo b
unsafeAbstractHashCbor = AbstractHash . Hash.hash . Cbor.serialize'

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2b_256

-- | Short version of 'unsafeHash'.
hash :: Bi a => a -> Hash a
hash = unsafeHash

-- | Short version of 'unsafeHash'.
hashCbor :: Cbor.Bi a => a -> Hash a
hashCbor = unsafeHashCbor

-- | Raw constructor application.
hashRaw :: ByteString -> Hash Raw
hashRaw = AbstractHash . Hash.hash

-- | Encode thing as 'Bi' data and then wrap into constructor.
unsafeHash :: Bi a => a -> Hash b
unsafeHash = unsafeAbstractHash

-- | Encode thing as 'Bi' data and then wrap into constructor.
unsafeHashCbor :: Cbor.Bi a => a -> Hash b
unsafeHashCbor = unsafeAbstractHashCbor

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

-- | Instances for `Raw` hashes for ease of casting
instance CastHash Raw a
instance CastHash a Raw

-- | Lenses for 'WithHash'
makeLensesFor [("whData", "_whData"), ("whHash", "_whHash")] ''WithHash
