{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Universum

import qualified Pos.Communication.Constants    as Const
import           Pos.Communication.Limits.Types (Limit (..),
                                                 MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key)
instance MessageLimited (ReqMsg key)
instance MessageLimited (MempoolMsg tag)

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData key contents) where
    getMsgLenLimit _ = do
        Limit invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        Limit dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (invLim `max` dataLim))

----------------------------------------------------------------------------
-- Instances of MessageLimitedPure for the relay types.
----------------------------------------------------------------------------

instance MessageLimitedPure (InvMsg key) where
    msgLenLimit = Limit Const.maxInvSize

instance MessageLimitedPure (ReqMsg key) where
    msgLenLimit = Limit Const.maxReqSize

instance MessageLimitedPure (MempoolMsg tag) where
    msgLenLimit = Limit Const.maxMempoolMsgSize
