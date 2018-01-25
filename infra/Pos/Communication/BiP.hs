{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       , bipPacking
       ) where

import           Universum

import           Control.Monad.ST
import qualified Data.ByteString.Lazy as LBS

import           Node.Message.Class (Packing (..), PackingType (..), Serializable (..))
import qualified Node.Message.Decoder as TW

import           Pos.Binary.Class (Bi (..))
import qualified Pos.Binary.Class as Bi

data BiP = BiP

instance PackingType BiP where
    type PackM BiP   = Identity
    type UnpackM BiP = ST RealWorld

bipPacking :: MonadIO m => Packing BiP m
bipPacking = Packing
    { packingType = Proxy @BiP
    , packM = pure . runIdentity
    , unpackM = liftIO . Control.Monad.ST.stToIO
    }

biPackMsg :: Bi.Encoding -> LBS.ByteString
biPackMsg = Bi.toLazyByteString

biUnpackMsg :: Bi t => Bi.Decoder RealWorld t -> TW.Decoder (UnpackM BiP) t
biUnpackMsg decoder = TW.Decoder (fromBiDecoder Proxy (Bi.deserialiseIncremental decoder))

instance  Bi t => Serializable BiP t where
    packMsg _   = pure . biPackMsg . Bi.encode
    unpackMsg _ = biUnpackMsg Bi.decode

type M = ST RealWorld

fromBiDecoder :: Bi t => Proxy t -> M (Bi.IDecode RealWorld t) -> M (TW.DecoderStep M t)
fromBiDecoder p x = do
    nextStep <- x
    case nextStep of
      (Bi.Partial cont)    -> return $ TW.Partial $ \bs -> TW.Decoder $ fromBiDecoder p (cont bs)
      (Bi.Done bs off t)   -> return (TW.Done bs off t)
      (Bi.Fail bs off exn) -> do
          let msg = "fromBiDecoder failure for " <> label p <> ": " <> show exn <> ", leftover: " <> show bs
          return (TW.Fail bs off msg)
