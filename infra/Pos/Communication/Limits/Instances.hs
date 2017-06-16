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
import           Data.Binary.Get                (lookAhead)
import           Universum

import           Pos.Binary.Class               (getWord8)

import qualified Pos.Communication.Constants    as Const
import           Pos.Communication.Limits.Types (Limit (..), Limiter (..),
                                                 MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData,
                                                 MempoolMsg (..), ReqMsg)

----------------------------------------------------------------------------
-- Instances for Limiter
----------------------------------------------------------------------------

-- | Bounds `InvOrData`.
instance Limiter l => Limiter (Limit t, l) where
    limitGet (invLimit, dataLimits) parser = do
        lookAhead getWord8 >>= \case
            0   -> limitGet invLimit parser
            1   -> limitGet dataLimits parser
            tag -> fail ("get@InvOrData: invalid tag: " ++ show tag)

    addLimit a (l1, l2) = (a `addLimit` l1, a `addLimit` l2)

-- | Bounds `DataMsg` in `InvData`.
-- Limit depends on value of first byte, which should be in range @0..3@.
instance Limiter (Limit t, Limit t, Limit t, Limit t) where
    limitGet limits parser = do
        -- skip first byte which belongs to `InvOrData`
        tag <- fromIntegral <$> lookAhead (getWord8 *> getWord8)
        case (limits ^.. each) ^? ix tag of
            Nothing    -> fail ("get@DataMsg: invalid tag: " ++ show tag)
            Just limit -> limitGet limit parser

    addLimit a = each %~ addLimit a

----------------------------------------------------------------------------
-- Instances for MessageLimited
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key)
instance MessageLimited (ReqMsg key)
instance MessageLimited (MempoolMsg tag)

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData key contents) where
    type LimitType (InvOrData key contents) =
        ( LimitType (InvMsg key)
        , LimitType (DataMsg contents)
        )
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
