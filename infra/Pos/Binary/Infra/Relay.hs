{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Binary.Infra.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Communication.Types.Relay (InvMsg (..), MempoolMsg (..), ReqMsg (..),
                                                ResMsg (..))
import           Pos.Util.Util (cborError)

instance Bi key => Bi (InvMsg key) where
    encode = encode . imKey
    decode = InvMsg <$> decode

instance Bi key => Bi (ReqMsg key) where
    encode = encode . rmKey
    decode = ReqMsg <$> decode

instance Bi key => Bi (ResMsg key) where
    encode (ResMsg {..}) = encode (resKey, resOk)
    decode = uncurry ResMsg <$> decode

instance Typeable tag => Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    encode MempoolMsg = encode (228 :: Word8)
    decode = do
        x <- decode @Word8
        when (x /= 228) $ cborError "wrong byte"
        pure MempoolMsg
