{-# LANGUAGE TemplateHaskell #-}

-- | Helper data type for block, tx attributes.
-- Map with integer 1-byte keys, arbitrary-type polymorph values.
-- Needed primarily for partial serialization.
-- Values are either parsed and put to some constructor or left as unparsed.

module Pos.Data.Attributes
  ( Attributes(attrData, attrRemain)
  , getAttributes
  , putAttributes
  , mkAttributes
  ) where

import           Control.Monad.Fail  (MonadFail (fail))
import           Data.Binary.Get     (Get, getByteString, getWord32be, getWord8)
import qualified Data.Binary.Get     as G
import           Data.Binary.Put     (Put, putByteString, putWord32be, putWord8)
import qualified Data.ByteString     as BS
import           Data.DeriveTH       (derive, makeNFData)
import qualified Data.Map            as M
import           Data.SafeCopy       (SafeCopy (..), contain, safeGet, safePut)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, int, (%))
import           Prelude             (show)
import           Universum           hiding (putByteString, show)

mkAttributes :: h -> Attributes h
mkAttributes dat = Attributes dat BS.empty

data Attributes h = Attributes
    { -- Data, containing known keys (deserialized)
      attrData   :: h
      -- Unprased ByteString
    , attrRemain :: ByteString
    }
  deriving (Eq, Ord, Generic)

instance Show h => Show (Attributes h) where
    show Attributes {..} =
      "Attributes { data: " ++ show attrData
        ++ (if BS.null attrRemain
              then ""
              else ", remain: <" ++ show (BS.length attrRemain) ++ " bytes> }")

instance Buildable h => Buildable (Attributes h) where
    build Attributes {..} = if BS.null attrRemain
               then Buildable.build attrData
               else bprint ("Attributes { data: " % build % ", remain: <" % int % " bytes> }")
                      attrData (BS.length attrRemain)

instance Hashable h => Hashable (Attributes h)

instance ( SafeCopy h
         ) =>
         SafeCopy (Attributes h) where
    getCopy =
        contain $
        do attrData <- safeGet
           attrRemain <- safeGet
           return $! Attributes {..}
    putCopy Attributes {..} =
        contain $
        do safePut attrData
           safePut attrRemain

getAttributes
  :: (Word8 -> h -> Maybe (Get h))
  -> Word32
  -> h
  -> Get (Attributes h)
getAttributes keyGetMapper maxLen initData = do
    totalLen <- getWord32be
    when (maxLen > 0 && totalLen > maxLen) $
       fail $ "Attributes: wrong totalLen " ++ show totalLen
                   ++ " (maxLen=" ++ show maxLen ++ ")"
    read1 <- G.bytesRead
    let readWhileKnown dat = do
          ifM G.isEmpty (return dat) $ do
              key <- G.lookAhead getWord8
              case keyGetMapper key dat of
                Nothing -> return dat
                Just gh -> getWord8 >> gh >>= readWhileKnown
    attrData <- readWhileKnown initData
    read <- flip (-) read1 <$> G.bytesRead
    when (read > fromIntegral totalLen) $
       fail $ "Attributes: wrong total length field value, deserialized "
                   ++ show read ++ " bytes, totalLen=" ++ show totalLen
    attrRemain <- getByteString $ fromIntegral totalLen - fromIntegral read
    return $ Attributes {..}

putAttributes
  :: (h -> [(Word8, ByteString)])
  -> Attributes h
  -> Put
putAttributes putMapper Attributes {..} = do
    putWord32be totalLen
    mapM_ putAttr kvs
    putByteString attrRemain
  where
    putAttr (k, v) = putWord8 k *> putByteString v
    kvs = M.toAscList $ M.fromList $ putMapper attrData
    totalLen :: Word32
    totalLen = fromIntegral $ BS.length attrRemain
             + (foldr' (\(_, bs) s -> s + BS.length bs + 1) 0 kvs)

derive makeNFData ''Attributes
