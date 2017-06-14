{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Node.Message.Binary
    ( BinaryP (..)
    , binaryPackMsg
    , binaryUnpackMsg
    ) where

import qualified Data.Binary                   as Bin
import qualified Data.Binary.Put               as Bin
import qualified Data.Binary.Get               as Bin
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Node.Message.Decoder          (Decoder (..))
import           Node.Message.Class            (Serializable (..))

data BinaryP = BinaryP

binaryPackMsg :: Bin.Put -> LBS.ByteString
binaryPackMsg =
    BS.toLazyByteStringWith
        (BS.untrimmedStrategy 256 4096)
        LBS.empty
    . Bin.execPut

binaryUnpackMsg :: Bin.Get t -> Decoder t
binaryUnpackMsg get = fromBinaryDecoder (Bin.runGetIncremental get)

fromBinaryDecoder :: Bin.Decoder t -> Decoder t
fromBinaryDecoder (Bin.Done bs bo t) = Done bs bo t
fromBinaryDecoder (Bin.Fail bs bo err) = Fail bs bo (T.pack err)
fromBinaryDecoder (Bin.Partial k) = Partial (fromBinaryDecoder . k)

-- TBD some way to make custom serialization strategies.
-- Perhaps a new typeclass
--
--   Bin.Binary t => BinarySerializable t
--
-- with default binary packing/unpacking functions.
instance Bin.Binary t => Serializable BinaryP t where
    packMsg _ = binaryPackMsg . Bin.put
    unpackMsg _ = binaryUnpackMsg Bin.get
