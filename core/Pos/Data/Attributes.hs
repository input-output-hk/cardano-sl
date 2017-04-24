{-# LANGUAGE TemplateHaskell #-}

-- | Helper data type for block, tx attributes.
--
-- Map with integer 1-byte keys, arbitrary-type polymorph values.
-- Needed primarily for partial serialization. Values are either
-- parsed and put to some constructor or left as unparsed.

module Pos.Data.Attributes
       ( Attributes (..)
       , getAttributes
       , putAttributes
       , mkAttributes
       ) where

import           Universum

import qualified Base
import           Data.Binary.Get     (Get)
import qualified Data.Binary.Get     as G
import           Data.Binary.Put     (Put)
import qualified Data.ByteString     as BS
import           Data.Default        (Default (..))
import           Data.DeriveTH       (derive, makeNFData)
import qualified Data.Map            as M
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, int, (%))

import           Pos.Binary.Class    (getRemainingByteString, getWithLength,
                                      getWithLengthLimited, getWord8, putByteString,
                                      putWithLength, putWord8)

mkAttributes :: h -> Attributes h
mkAttributes dat = Attributes dat BS.empty

-- | Convenient wrapper for the datatype to represent it (in binary
-- format) as k-v map.
data Attributes h = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: h
      -- | Unparsed ByteString
    , attrRemain :: ByteString
    }
  deriving (Eq, Ord, Generic, Typeable)

instance Default h => Default (Attributes h) where
    def = mkAttributes def

instance Base.Show h => Base.Show (Attributes h) where
    show Attributes {..} =
        let remain | BS.null attrRemain = ""
                   | otherwise = ", remain: <" <> show (BS.length attrRemain) <> " bytes>"
        in mconcat [ "Attributes { data: ", show attrData, remain, " }"]

instance Buildable h => Buildable (Attributes h) where
    build Attributes {..} =
        if BS.null attrRemain
        then Buildable.build attrData
        else bprint ("Attributes { data: "%build%", remain: <"%int%" bytes> }")
               attrData (BS.length attrRemain)

instance Hashable h => Hashable (Attributes h)

-- | Generate 'Attributes' reader given mapper from keys to 'Get',
-- maximum input length and the attribute value 'h' itself.
--
-- The mapper will be applied until it returns 'Nothing'.
getAttributes :: (Word8 -> h -> Maybe (Get h))
              -> Maybe Word32
              -> h
              -> Get (Attributes h)
getAttributes keyGetMapper maxLen initData =
    maybeLimit $ do
        let readWhileKnown dat = ifM G.isEmpty (return dat) $ do
                key <- G.lookAhead getWord8
                case keyGetMapper key dat of
                    Nothing -> return dat
                    Just gh -> getWord8 >> gh >>= readWhileKnown
        attrData <- readWhileKnown initData
        attrRemain <- getRemainingByteString
        return $ Attributes {..}
  where
    maybeLimit act = case maxLen of
        Nothing -> getWithLength act
        Just l  -> getWithLengthLimited (fromIntegral l) act

-- | Generate 'Put' given the way to serialize inner attribute value
-- into set of keys and values.
putAttributes :: (h -> [(Word8, Put)]) -> Attributes h -> Put
putAttributes putMapper Attributes {..} =
    putWithLength $ do
        mapM_ putAttr kvs
        putByteString attrRemain
  where
    putAttr (k, v) = putWord8 k *> v
    kvs = M.toAscList $ M.fromList $ putMapper attrData

derive makeNFData ''Attributes
