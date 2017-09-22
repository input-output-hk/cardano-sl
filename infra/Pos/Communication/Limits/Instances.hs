{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Universum

import qualified Pos.Communication.Configuration as Conf
import           Pos.Communication.Limits.Types (Limit (..), MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg, ReqOrRes,
                                                 ResMsg)
import           Pos.Infra.Configuration        (HasInfraConfiguration)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
----------------------------------------------------------------------------

instance HasInfraConfiguration => MessageLimited (InvMsg key)
instance HasInfraConfiguration => MessageLimited (ReqMsg key)
instance HasInfraConfiguration => MessageLimited (ResMsg key)
instance HasInfraConfiguration => MessageLimited (MempoolMsg tag)

instance (HasInfraConfiguration, MessageLimited (DataMsg contents))
      => MessageLimited (InvOrData key contents) where
    getMsgLenLimit _ = do
        Limit invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        Limit dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (invLim `max` dataLim))

instance HasInfraConfiguration => MessageLimited (ReqOrRes key) where
    getMsgLenLimit _ = do
        Limit reqLim <- getMsgLenLimit $ Proxy @(ReqMsg key)
        Limit resLim <- getMsgLenLimit $ Proxy @(ResMsg key)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (reqLim `max` resLim))

----------------------------------------------------------------------------
-- Instances of MessageLimitedPure for the relay types.
----------------------------------------------------------------------------

instance HasInfraConfiguration => MessageLimitedPure (InvMsg key) where
    msgLenLimit = Limit Conf.maxInvSize

instance HasInfraConfiguration => MessageLimitedPure (ReqMsg key) where
    -- Add 1 because ReqMsg contains a 'Maybe key'
    msgLenLimit = Limit (Conf.maxReqSize + 1)

instance HasInfraConfiguration => MessageLimitedPure (ResMsg key) where
    -- It's a ResMsg key, with an extra bool, and overhead for the tuple.
    msgLenLimit = Limit (Conf.maxReqSize + 2)

instance HasInfraConfiguration => MessageLimitedPure (MempoolMsg tag) where
    msgLenLimit = Limit Conf.maxMempoolMsgSize
