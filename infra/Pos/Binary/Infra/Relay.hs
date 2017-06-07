module Pos.Binary.Infra.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi, get, getWord8, label, put, putWord8)
import           Pos.Communication.Types.Relay (InvMsg (..), MempoolMsg (..), ReqMsg (..))

instance (Bi key) => Bi (InvMsg key) where
    put InvMsg {..} = put imKey
    get = label "InvMsg" $ InvMsg <$> get

instance (Bi key) => Bi (ReqMsg key) where
    put ReqMsg {..} = put rmKey
    get = label "ReqMsg" $ ReqMsg <$> get

instance Bi (MempoolMsg tag) where
    -- The extra byte is needed because time-warp doesn't work with
    -- possibly-empty messages. 228 was chosen as homage to @pva701
    put MempoolMsg = putWord8 228
    get = label "MempoolMsg" $ do
        x <- getWord8
        when (x /= 228) $ fail "wrong byte"
        pure MempoolMsg
