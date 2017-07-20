{-# LANGUAGE TypeFamilies          #-}

-- To be removed
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP
       ) where

import           Universum

import           Control.Monad.ST
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Node.Message.Class   (Packing (..), PackingType (..), Serializable (..))
import qualified Node.Message.Decoder as TW

import qualified Codec.CBOR.Decoding  as D
import qualified Codec.CBOR.Encoding  as CBOR
import qualified Codec.CBOR.Read      as CBOR
import qualified Codec.CBOR.Write     as CBOR
import           Pos.Binary.Class     (Bi (..))
import qualified Pos.Binary.Class     as Bi

data BiP

instance PackingType BiP where
    type PackM BiP   = Identity
    type UnpackM BiP = ST RealWorld

bipPacking :: Packing BiP IO
bipPacking = Packing
    { packingType = Proxy @BiP
    , packM = pure . runIdentity
    , unpackM = Control.Monad.ST.stToIO
    }

biPackMsg :: CBOR.Encoding -> LBS.ByteString
biPackMsg = CBOR.toLazyByteString

biUnpackMsg :: Bi t => D.Decoder RealWorld t -> TW.Decoder (UnpackM BiP) t
biUnpackMsg decoder = TW.Decoder (fromBiDecoder (CBOR.deserialiseIncremental decoder))

instance  Bi t => Serializable BiP t where
    packMsg _   = pure . biPackMsg . Bi.encode
    unpackMsg _ = biUnpackMsg Bi.decode

type M = ST RealWorld

fromBiDecoder :: Bi t => M (CBOR.IDecode RealWorld t) -> M (TW.DecoderStep M t)
fromBiDecoder x = do
    nextStep <- x
    case nextStep of
      (CBOR.Partial cont)    -> return $ TW.Partial $ \bs -> TW.Decoder $ fromBiDecoder (cont bs)
      (CBOR.Done bs off t)   -> return (TW.Done bs off t)
      (CBOR.Fail bs off exn) -> return (TW.Fail bs off (toText @String $ Universum.show exn))
