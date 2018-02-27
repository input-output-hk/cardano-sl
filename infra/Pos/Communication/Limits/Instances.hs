{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Universum

import qualified Pos.Communication.Constants as Const
import           Pos.Communication.Limits.Types (Limit (..), MessageLimited (..))
import           Pos.Communication.Types.Relay (InvMsg, MempoolMsg (..), ReqMsg, ResMsg)

----------------------------------------------------------------------------
-- Instances of MessageLimited for the relay types.
----------------------------------------------------------------------------

instance (Applicative m) => MessageLimited (InvMsg key) m where
    getMsgLenLimit _ = pure $ Limit Const.maxInvSize

instance (Applicative m) => MessageLimited (ReqMsg key) m where
    -- Add 1 because ReqMsg contains a 'Maybe key'
    getMsgLenLimit _ = pure $ Limit (Const.maxReqSize + 1)

instance (Applicative m) => MessageLimited (ResMsg key) m where
    -- It's a ResMsg key, with an extra bool, and overhead for the tuple.
    getMsgLenLimit _ = pure $ Limit (Const.maxReqSize + 2)

instance (Applicative m) => MessageLimited (MempoolMsg tag) m where
    getMsgLenLimit _ = pure $ Limit Const.maxMempoolMsgSize
