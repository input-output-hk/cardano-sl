module Pos.Binary.Infra.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), getWord8, label, labelS,
                                                putConst, putField)
import qualified Pos.Binary.Cbor               as Cbor
import           Pos.Communication.Types.Relay (InvMsg (..), MempoolMsg (..), ReqMsg (..))

instance Bi key => Bi (InvMsg key) where
    sizeNPut = labelS "InvMsg" $ putField imKey
    get = label "InvMsg" $ InvMsg <$> get

instance Cbor.Bi key => Cbor.Bi (InvMsg key) where
  encode = Cbor.encode . imKey
  decode = InvMsg <$> Cbor.decode

instance Bi key => Bi (ReqMsg key) where
    sizeNPut = labelS "ReqMsg" $ putField rmKey
    get = label "ReqMsg" $ ReqMsg <$> get

instance Cbor.Bi key => Cbor.Bi (ReqMsg key) where
  encode = Cbor.encode . rmKey
  decode = ReqMsg <$> Cbor.decode

instance Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    sizeNPut = labelS "MempoolMsg" $ putConst @Word8 228
    get = label "MempoolMsg" $ do
        x <- getWord8
        when (x /= 228) $ fail "wrong byte"
        pure MempoolMsg

instance Cbor.Bi (MempoolMsg tag) where
  encode MempoolMsg = Cbor.encode (228 :: Word8)
  decode = do
    x <- Cbor.decode @Word8
    when (x /= 228) $ fail "wrong byte"
    pure MempoolMsg
