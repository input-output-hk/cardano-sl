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
import           Pos.Communication.Limits.Types (Limit (..), MessageLimited (..))
import           Pos.Communication.Types.Relay (InvMsg, MempoolMsg (..), ReqMsg, ResMsg)
import           Pos.Infra.Configuration (HasInfraConfiguration)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
----------------------------------------------------------------------------

-- This is the Either a b instance (overlaps, InvOrData is Either).
{-
instance (HasInfraConfiguration, MessageLimited (DataMsg contents) m, Monad m)
      => MessageLimited (InvOrData key contents) m where
    getMsgLenLimit _ = do
        Limit invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        Limit dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (invLim `max` dataLim))
-}

-- This is the Either a b instance (overlaps, ReqOrRes is Either).
{-
instance (HasInfraConfiguration, Monad m) => MessageLimited (ReqOrRes key) m where
    getMsgLenLimit _ = do
        Limit reqLim <- getMsgLenLimit $ Proxy @(ReqMsg key)
        Limit resLim <- getMsgLenLimit $ Proxy @(ResMsg key)
        -- 1 byte is added because of `Either`
        return $ Limit (1 + (reqLim `max` resLim))
-}

----------------------------------------------------------------------------
-- Instances of MessageLimitedPure for the relay types.
----------------------------------------------------------------------------

instance (HasInfraConfiguration, Applicative m) => MessageLimited (InvMsg key) m where
    getMsgLenLimit _ = pure $ Limit Conf.maxInvSize

instance (HasInfraConfiguration, Applicative m) => MessageLimited (ReqMsg key) m where
    -- Add 1 because ReqMsg contains a 'Maybe key'
    getMsgLenLimit _ = pure $ Limit (Conf.maxReqSize + 1)

instance (HasInfraConfiguration, Applicative m) => MessageLimited (ResMsg key) m where
    -- It's a ResMsg key, with an extra bool, and overhead for the tuple.
    getMsgLenLimit _ = pure $ Limit (Conf.maxReqSize + 2)

instance (HasInfraConfiguration, Applicative m) => MessageLimited (MempoolMsg tag) m where
    getMsgLenLimit _ = pure $ Limit Conf.maxMempoolMsgSize
