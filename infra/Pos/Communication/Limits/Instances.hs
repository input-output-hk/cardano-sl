{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Universum

import qualified Pos.Communication.Constants    as Const
import           Pos.Communication.Limits.Types (Limit (..), MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg, ReqOrRes,
                                                 ResMsg)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key)
instance MessageLimited (ReqMsg key)
instance MessageLimited (ResMsg key)
instance MessageLimited (MempoolMsg tag)

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData key contents) where
    getMsgLenLimit _ = do
        Limit invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        Limit dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (invLim `max` dataLim))

instance MessageLimited (ReqOrRes key) where
    getMsgLenLimit _ = do
        Limit reqLim <- getMsgLenLimit $ Proxy @(ReqMsg key)
        Limit resLim <- getMsgLenLimit $ Proxy @(ResMsg key)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (reqLim `max` resLim))

----------------------------------------------------------------------------
-- Instances of MessageLimitedPure for the relay types.
----------------------------------------------------------------------------

instance MessageLimitedPure (InvMsg key) where
    msgLenLimit = Limit Const.maxInvSize

instance MessageLimitedPure (ReqMsg key) where
    -- Add 1 because ReqMsg contains a 'Maybe key'
    msgLenLimit = Limit (Const.maxReqSize + 1)

instance MessageLimitedPure (ResMsg key) where
    -- It's a ResMsg key, with an extra bool, and overhead for the tuple.
    msgLenLimit = Limit (Const.maxReqSize + 2)

instance MessageLimitedPure (MempoolMsg tag) where
    msgLenLimit = Limit Const.maxMempoolMsgSize
