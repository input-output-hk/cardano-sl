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
import           Pos.Communication.Limits.Types (Limit (..), Limiter (..),
                                                 MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg)

import qualified Pos.Binary.Class               as Bi
newtype EitherLimiter a b = EitherLimiter (a, b)

----------------------------------------------------------------------------
-- Instances for Limiter
----------------------------------------------------------------------------

-- | Bounds `InvOrData`.
instance (Limiter l, Limiter t) => Limiter (EitherLimiter l t) where
    limitGet (EitherLimiter (invLimit, dataLimits)) parser = do
        Bi.lookAhead Bi.getWord8 >>= \case
            0   -> limitGet invLimit parser
            1   -> limitGet dataLimits parser
            tag -> fail ("EitherLimiter: invalid tag: " ++ show tag)

    addLimit a (EitherLimiter (l1, l2)) = EitherLimiter (a `addLimit` l1, a `addLimit` l2)

----------------------------------------------------------------------------
-- Instances for MessageLimited
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key)
instance MessageLimited (ReqMsg key)
instance MessageLimited (MempoolMsg tag)

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData key contents) where
    type LimitType (InvOrData key contents) =
        EitherLimiter (LimitType (InvMsg key)) (LimitType (DataMsg contents))
    getMsgLenLimit _ = do
        invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return $ EitherLimiter (1 `addLimit` invLim, 1 `addLimit` dataLim)

----------------------------------------------------------------------------
-- Instances for MessageLimitedPure
----------------------------------------------------------------------------

instance MessageLimitedPure (InvMsg key) where
    msgLenLimit = Limit Const.maxInvSize

instance MessageLimitedPure (ReqMsg key) where
    msgLenLimit = Limit Const.maxReqSize

instance MessageLimitedPure (MempoolMsg tag) where
    msgLenLimit = Limit Const.maxMempoolMsgSize
