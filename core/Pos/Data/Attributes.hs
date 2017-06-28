-- | Helper data type for block, tx attributes.
--
-- Map with integer 1-byte keys, arbitrary-type polymorph values.
-- Needed primarily for partial serialization. Values are either
-- parsed and put to some constructor or left as unparsed.

module Pos.Data.Attributes
       ( Attributes (..)
       , areAttributesKnown
       , getAttributes
       , putAttributes
       , putAttributesS
       , sizeAttributes
       , mkAttributes
       ) where

import           Universum

import qualified Data.ByteString     as BS
import           Data.Default        (Default (..))
import           Data.DeriveTH       (derive, makeNFData)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, int, (%))
import qualified Prelude

import           Pos.Binary.Class    (Peek, Poke, PokeWithSize (..), getBytes,
                                      getPeekLength, getWithLength, getWithLengthLimited,
                                      getWord8, lookAhead, putBytesS, putWithLengthS,
                                      putWord8S)

mkAttributes :: h -> Attributes h
mkAttributes dat = Attributes dat BS.empty

-- | Convenient wrapper for the datatype to represent it (in binary
-- format) as k-v map.
data Attributes h = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: h
      -- | Unparsed ByteString
    , attrRemain :: ByteString
    } deriving (Eq, Ord, Generic, Typeable)

instance Default h => Default (Attributes h) where
    def = mkAttributes def

instance Show h => Show (Attributes h) where
    show Attributes {..} =
        let remain | BS.null attrRemain = ""
                   | otherwise = ", remain: <" <> show (BS.length attrRemain) <> " bytes>"
        in mconcat [ "Attributes { data: ", show attrData, remain, " }"]

instance {-# OVERLAPPABLE #-} Buildable h => Buildable (Attributes h) where
    build Attributes {..} =
        if BS.null attrRemain
        then Buildable.build attrData
        else bprint ("Attributes { data: "%build%", remain: <"%int%" bytes> }")
               attrData (BS.length attrRemain)

instance Buildable (Attributes ()) where
    build Attributes {..}
        | null attrRemain = "<no attributes>"
        | otherwise =
            bprint
                ("Attributes { data: (), remain: <"%int%" bytes> }")
                (length attrRemain)

instance Hashable h => Hashable (Attributes h)

-- | Check whether all data from 'Attributes' is known, i. e. was
-- successfully parsed into some structured data.
areAttributesKnown :: Attributes __ -> Bool
areAttributesKnown = null . attrRemain

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

-- | Generate 'Attributes' reader given mapper from keys to 'Get',
-- maximum input length and the attribute value 'h' itself.
--
-- The mapper will be applied until it returns 'Nothing'.
getAttributes
    :: (Word8 -> h -> Maybe (Peek h))   -- ^ A function to parse an attribute
                                        --    with given key and “set” it in
                                        --    the attributes structure
    -> Maybe Word32                     -- ^ Maximum length of the structure
    -> h                                -- ^ Default data (which read
                                        --    attributes are applied to)
    -> Peek (Attributes h)
getAttributes keyGetMapper maxLen initData = maybeLimit $ \len -> do
    let readWhileKnown remaining dat
          | remaining < 0 = fail "getAttributes: read more bytes than expected"
          | remaining == 0 = pure (dat, remaining)
          | otherwise = do
                key <- lookAhead getWord8
                case keyGetMapper key dat of
                    -- the attribute is unknown, so we finish reading
                    Nothing -> pure (dat, remaining)
                    -- the attribute is known, so we proceed with reading
                    Just gh -> do
                        (dat', read) <- getPeekLength (getWord8 >> gh)
                        readWhileKnown (remaining - fromIntegral read) dat'
    (attrData, remaining) <- readWhileKnown len initData
    -- It's important that we use 'getBytes' here and not 'get'.
    -- See the note above.
    attrRemain <- getBytes (fromIntegral remaining)
    pure $ Attributes {..}
 where
   maybeLimit act = case maxLen of
       Nothing -> getWithLength act
       Just l  -> getWithLengthLimited (fromIntegral l) act

-- | Generate 'Put' given the way to serialize inner attribute value
-- into set of keys and values.
putAttributes :: (h -> [(Word8, PokeWithSize ())]) -> Attributes h -> Poke ()
putAttributes putMapper attrs = pwsToPoke (putAttributesS putMapper attrs)

sizeAttributes :: (h -> [(Word8, PokeWithSize ())]) -> Attributes h -> Int
sizeAttributes putMapper attrs = pwsToSize (putAttributesS putMapper attrs)

putAttributesS :: (h -> [(Word8, PokeWithSize ())]) -> Attributes h -> PokeWithSize ()
putAttributesS putMapper Attributes {..} =
    putWithLengthS $
        traverse_ putAttr kvs *>
        -- Note: it's important that we use 'putBytesS' here and not 'putS'.
        -- See the note above.
        putBytesS attrRemain
 where
   putAttr (k, v) = putWord8S k *> v
   kvs = sortOn fst $ putMapper attrData

derive makeNFData ''Attributes
