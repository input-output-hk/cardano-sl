-- | Helper data type for block, tx attributes.
--
-- Map with integer 1-byte keys, arbitrary-type polymorph values.
-- Needed primarily for partial serialization. Values are either
-- parsed and put to some constructor or left as unparsed.

module Pos.Data.Attributes
       ( UnparsedFields(..)
       , fromRaw
       , toRaw
       , Attributes (..)
       , areAttributesKnown
       , encodeAttributes
       , decodeAttributes
       , mkAttributes
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Default (Default (..))
import qualified Data.Hashable as H
import qualified Data.Map as M
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, (%))
import qualified Prelude

import           Pos.Binary.Class

-- | Representation of unparsed fields in Attributes. Newtype wrapper is used
-- for clear backward compatibility between previous representation (which was
-- just a single ByteString) during transition from Store to CBOR.
newtype UnparsedFields = UnparsedFields (Map Word8 ByteString)
    deriving (Eq, Ord, Show, Generic, Typeable, NFData)

instance Hashable UnparsedFields where
    hashWithSalt salt = H.hashWithSalt salt . M.toList . fromUnparsedFields

fromUnparsedFields :: UnparsedFields -> Map Word8 ByteString
fromUnparsedFields (UnparsedFields m) = m

-- | Convert from Store format.
fromRaw :: BS.ByteString -> UnparsedFields
fromRaw = \case
    "" -> UnparsedFields $ M.empty
    bs -> UnparsedFields $ M.singleton maxBound bs

-- | Extract Store format.
toRaw :: UnparsedFields -> BS.ByteString
toRaw = fromMaybe BS.empty . M.lookup maxBound . fromUnparsedFields

----------------------------------------

mkAttributes :: h -> Attributes h
mkAttributes dat = Attributes dat (UnparsedFields M.empty)

-- | Convenient wrapper for the datatype to represent it (in binary
-- format) as k-v map.
data Attributes h = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: h
      -- | Remaining, unparsed fields.
    , attrRemain :: UnparsedFields
    } deriving (Eq, Ord, Generic, Typeable)

instance Default h => Default (Attributes h) where
    def = mkAttributes def

instance Show h => Show (Attributes h) where
    show attr@Attributes {..} =
        let remain | areAttributesKnown attr = ""
                   | otherwise = ", remain: <" <> show (unknownAttributesLength attr) <> " bytes>"
        in mconcat [ "Attributes { data: ", show attrData, remain, " }"]

instance {-# OVERLAPPABLE #-} Buildable h => Buildable (Attributes h) where
    build attr@Attributes {..} =
        if areAttributesKnown attr
        then Buildable.build attrData
        else bprint ("Attributes { data: "%build%", remain: <"%int%" bytes> }")
               attrData (unknownAttributesLength attr)

instance Buildable (Attributes ()) where
    build attr
        | areAttributesKnown attr = "<no attributes>"
        | otherwise =
            bprint
                ("Attributes { data: (), remain: <"%int%" bytes> }")
                (unknownAttributesLength attr)

instance Hashable h => Hashable (Attributes h)

instance NFData h => NFData (Attributes h)

-- | Check whether all data from 'Attributes' is known, i. e. was
-- successfully parsed into some structured data.
areAttributesKnown :: Attributes __ -> Bool
areAttributesKnown = M.null . fromUnparsedFields . attrRemain

unknownAttributesLength :: Attributes __ -> Int
unknownAttributesLength = sum . map BS.length . fromUnparsedFields . attrRemain

{- NOTE: Attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Attributes are a way to add fields to datatypes while maintaining backwards
compatibility. Suppose that you have this datatype:

    data Foo = Foo {
        x :: Int,
        y :: Int,
        attrs :: Attributes FooAttrs }

@Attributes FooAttrs@ is a key-value map that deserializes into @FooAttrs@.
Each key is a single byte, and each value is an arbitary bytestring. It's
serialized like this:

    <length of following data>
    <k1><first attribute>
    <k2><second attribute>
    <attrRemain>

The attributes are read as long as their keys are “known” (i.e. as long as we
know how to interpret those keys), and the rest is stored separately. For
instance, let's say that in first version of CSL, @FooAttrs@ looks like this:

    data FooAttrs = FooAttrs {
        foo :: Text,
        bar :: [Int] }

It would be serialized as follows:

    <length> <0x00><foo> <0x01><bar>

In the next version of CSL we add a new field @quux@. The new version would
serialize it like this:

    <length> <0x00><foo> <0x01><bar> <0x02><quux>

And the old version would treat it like this:

    <length> <0x00><foo> <0x01><bar> <attrRemain>

This way the old version can serialize and deserialize data received from the
new version in a lossless way (i.e. when the old version does serialization
it would just put @attrRemain@ back after other attributes and the new
version would be able to parse it).

-}

encodeAttributes
    :: forall t. [(Word8, t -> BS.ByteString)]
    -> Attributes t
    -> Encoding
encodeAttributes encs Attributes{..} =
    encode $ foldr go (fromUnparsedFields attrRemain) encs
  where
    go :: (Word8, t -> BS.ByteString)
       -> Map Word8 BS.ByteString
       -> Map Word8 BS.ByteString
    go (k, f) = M.alter (insertCheck $ f attrData) k
        where
          insertCheck v Nothing   = Just v
          insertCheck _ (Just v') = error $ "encodeAttributes: impossible: field no. "
              <> show k <> " is already encoded as unparsed field: " <> show v'

decodeAttributes
    :: forall t s. t
    -> (Word8 -> BS.ByteString -> t -> Decoder s (Maybe t))
    -> Decoder s (Attributes t)
decodeAttributes initval updater = do
    raw <- decode @(Map Word8 BS.ByteString)
    foldrM go (Attributes initval $ UnparsedFields raw) $ M.toList raw
  where
    go :: (Word8, BS.ByteString) -> Attributes t -> Decoder s (Attributes t)
    go (k, v) attr@Attributes{..} = do
        updaterData <- updater k v attrData
        pure $ case updaterData of
            Nothing      -> attr
            Just newData -> Attributes
                { attrData   = newData
                , attrRemain = UnparsedFields . M.delete k $ fromUnparsedFields attrRemain
                }
