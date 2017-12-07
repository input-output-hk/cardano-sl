{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Node.Message.Binary
    ( BinaryP
    , binaryP
    , binaryPacking
    , binaryPackMsg
    , binaryUnpackMsg
    ) where

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor.Identity (Identity (..))
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Node.Message.Class (Packing (..), PackingType (..), Serializable (..))
import           Node.Message.Decoder (Decoder (..), DecoderStep (..))

data BinaryP

binaryP :: Proxy BinaryP
binaryP = Proxy

-- | BinaryP packing works in any Applicative.
binaryPacking :: ( Applicative m ) => Packing BinaryP m
binaryPacking = Packing
    { packingType = binaryP
    , packM = pure . runIdentity
    , unpackM = pure . runIdentity
    }

instance PackingType BinaryP where
    type PackM BinaryP = Identity
    type UnpackM BinaryP = Identity

binaryPackMsg :: Bin.Put -> LBS.ByteString
binaryPackMsg =
    BS.toLazyByteStringWith
        (BS.untrimmedStrategy 256 4096)
        LBS.empty
    . Bin.execPut

binaryUnpackMsg :: Bin.Get t -> Decoder (UnpackM BinaryP) t
binaryUnpackMsg get = Decoder (pure (fromBinaryDecoder (Bin.runGetIncremental get)))

fromBinaryDecoder :: Bin.Decoder t -> DecoderStep (UnpackM BinaryP) t
fromBinaryDecoder (Bin.Done bs bo t)   = Done bs bo t
fromBinaryDecoder (Bin.Fail bs bo err) = Fail bs bo (T.pack err)
fromBinaryDecoder (Bin.Partial k)      = Partial (Decoder . pure . fromBinaryDecoder . k)

instance ( Bin.Binary t ) => Serializable BinaryP t where
    packMsg _ = pure . binaryPackMsg . Bin.put
    unpackMsg _ = binaryUnpackMsg Bin.get
