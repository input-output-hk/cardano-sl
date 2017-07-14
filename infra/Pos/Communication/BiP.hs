{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       ) where

import           Universum

import qualified Data.ByteString            as BS

import           Node.Message.Class         (Serializable (..))
import           Node.Message.Decoder       (Decoder (..))

import           Pos.Binary.Class           (Bi (..), serialize, decodeFull)

data BiP = BiP

instance  Bi t => Serializable BiP t where
    packMsg _ t = serialize t
    unpackMsg _ = cborDecoder

-- CSL-1296: Check that this new cborDecoder is sound, as it doesn't use
-- lenght-prefix and deserialize everything in one gulp.
cborDecoder :: Bi t => Decoder t
cborDecoder = Partial $ \mbs -> case mbs of
    Nothing -> Fail BS.empty 0 "Unexpected end of input"
    Just bs'  -> case (decodeFull bs') of
      Left ex -> Fail BS.empty (fromIntegral (BS.length bs')) ex
      Right v -> Done BS.empty (fromIntegral (BS.length bs')) v
