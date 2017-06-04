{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Control.Lens                   (each, ix)
import qualified Data.Binary                    as Bin
import           Universum

import           Pos.Binary.Class               (getWord8)

import qualified Pos.Communication.Constants    as Const
import           Pos.Communication.Limits.Types (Limit (..), Limiter (..),
                                                 MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg)

newtype EitherLimiter a b = EitherLimiter (a, b)

----------------------------------------------------------------------------
-- Instances for Limiter
----------------------------------------------------------------------------

instance (Limiter l, Limiter t) => Limiter (EitherLimiter l t) where
    sizeGet (leftLim, rightLim) = Bin.getWord8 >>= \case
        0 -> sizeGet leftLim
        1 -> sizeGet rightLim
        t -> fail $ "Failed to read tag " ++ show t

    addLimit a (l1, l2) = (a `addLimit` l1, a `addLimit` l2)

----------------------------------------------------------------------------
-- Instances for MessageLimited
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key)
instance MessageLimited (ReqMsg key)
instance MessageLimited (MempoolMsg tag)

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData key contents) where
    type LimitType (InvOrData key contents) =
        EitherLimiter (InvMsg key) (DataMsg contents)
    getMsgLenLimit _ = do
        invLim  <- getMsgLenLimit $ Proxy @(InvMsg key)
        dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return (1 `addLimit` invLim, 1 `addLimit` dataLim)

----------------------------------------------------------------------------
-- Instances for MessageLimitedPure
----------------------------------------------------------------------------

instance MessageLimitedPure (InvMsg key) where
    msgLenLimit = Limit Const.maxInvSize

instance MessageLimitedPure (ReqMsg key) where
    msgLenLimit = Limit Const.maxReqSize

instance MessageLimitedPure (MempoolMsg tag) where
    msgLenLimit = Limit Const.maxMempoolMsgSize
