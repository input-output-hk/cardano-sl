{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

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
       , decodeAbstractHash
       , decodeHash
       , abstractHash
       , unsafeAbstractHash
       , unsafeMkAbstractHash

         -- * Common Hash
       , Hash
       , AHash(..)
       , hashHexF
       , mediumHashF
       , shortHashF
       , hash
       , hashRaw
       , hashRaw'
       , unsafeHash
       , unsafeCheatingHashCoerce

         -- * Utility
       , CastHash (castHash)
       , HashAlgorithm
       , hashDigestSize'
       , reifyHashDigestSize
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Crypto.Hash (Blake2b_256, Digest, HashAlgorithm, hashDigestSize)
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable (hashWithSalt), hashPtrWithSalt)
import           Data.Reflection (reifyNat)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, fitLeft, later, (%.))
import qualified Prelude
import qualified Serokell.Util.Base16 as B16
import           System.IO.Unsafe (unsafeDupablePerformIO)

import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Binary.Class as Bi

----------------------------------------------------------------------------
-- WithHash
----------------------------------------------------------------------------

data WithHash a = WithHash
    { whData :: a
    , whHash :: Hash a
    } deriving (Show, Typeable, Generic)

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
    build = bprint mediumHashF

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
decodeAbstractHash ::
       forall algo a. HashAlgorithm algo
    => Text
    -> Either Text (AbstractHash algo a)
decodeAbstractHash prettyHash = do
    bytes <- B16.decode prettyHash
    case Hash.digestFromByteString bytes of
        Nothing ->
            Left
                ("decodeAbstractHash: " <> "can't convert bytes to hash," <>
                 " the value was " <> prettyHash)
        Just digest -> return (AbstractHash digest)

-- | Parses given hash in base16 form.
decodeHash :: Bi (Hash a) => Text -> Either Text (Hash a)
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
unsafeAbstractHash = AbstractHash . Hash.hashlazy . Bi.serialize

-- | Make an AbstractHash from a lazy ByteString. You can choose the phantom
-- type, hence the "unsafe".
-- The other 'unsafeAbstractHash' actually serializes the thing, fulfilling
-- the purpose of the second type parameter, and so is not actually "unsafe".
unsafeMkAbstractHash
    :: (HashAlgorithm algo)
    => LBS.ByteString -> AbstractHash algo anything
unsafeMkAbstractHash = AbstractHash . Hash.hashlazy

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2b_256

-- | Short version of 'unsafeHash'.
hash :: Bi a => a -> Hash a
hash = unsafeHash

-- | Raw constructor application.
hashRaw :: LBS.ByteString -> Hash Raw
hashRaw = AbstractHash . Hash.hashlazy

hashRaw' :: ByteString -> Hash Raw
hashRaw' = AbstractHash . Hash.hash

-- | Encode thing as 'Bi' data and then wrap into constructor.
unsafeHash :: Bi a => a -> Hash b
unsafeHash = unsafeAbstractHash

-- | Specialized formatter for 'Hash'.
hashHexF :: Format r (AbstractHash algo a -> r)
hashHexF = later $ \(AbstractHash x) -> Buildable.build (show x :: Text)

-- | Smart formatter for 'Hash' to show only first @16@ characters of 'Hash'.
mediumHashF :: Format r (AbstractHash algo a -> r)
mediumHashF = fitLeft 16 %. hashHexF

-- | Smart formatter for 'Hash' to show only first @8@ characters of 'Hash'.
shortHashF :: Format r (AbstractHash algo a -> r)
shortHashF = fitLeft 8 %. hashHexF

newtype AHash = AHash { getAHash :: forall a. Hash a }

deriving instance Eq AHash
deriving instance Ord AHash
deriving instance Show AHash
deriving instance Buildable AHash

-- | This instance is severe abuse of the fact that the 'a'
-- parameter of 'AbstractHash' is phantom. It intentionally
-- confuses existential quantification with universal.
unsafeCheatingHashCoerce :: Hash a -> AHash
unsafeCheatingHashCoerce h = AHash (coerce h)

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
