{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Communication.Limits.Instances
       (
       ) where

import           Control.Lens                   (both, each, ix)
import           Data.Binary.Get                (getWord8, lookAhead)
import           Data.Proxy                     (Proxy (..))
import           Universum

import qualified Pos.Communication.Constants    as Const
import           Pos.Communication.Limits.Types (Limit (..), Limiter (..),
                                                 MessageLimited (..),
                                                 MessageLimitedPure (..))
import           Pos.Communication.Types.Relay  (DataMsg (..), InvMsg, InvOrData, ReqMsg)

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

    addLimit a = both %~ addLimit a

----------------------------------------------------------------------------
-- Instances for MessageLimited
----------------------------------------------------------------------------

instance MessageLimited (InvMsg key tag) where
    type LimitType (InvMsg key tag) = Limit (InvMsg key tag)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (ReqMsg key tag) where
    type LimitType (ReqMsg key tag) = Limit (ReqMsg key tag)
    getMsgLenLimit _ = return msgLenLimit

instance MessageLimited (DataMsg contents)
      => MessageLimited (InvOrData tag key contents) where
    type LimitType (InvOrData tag key contents) =
        ( LimitType (InvMsg key tag)
        , LimitType (DataMsg contents)
        )
    getMsgLenLimit _ = do
        invLim  <- getMsgLenLimit $ Proxy @(InvMsg key tag)
        dataLim <- getMsgLenLimit $ Proxy @(DataMsg contents)
        -- 1 byte is added because of `Either`
        return (1 `addLimit` invLim, 1 `addLimit` dataLim)

----------------------------------------------------------------------------
-- Instances for MessageLimitedPure
----------------------------------------------------------------------------

instance MessageLimitedPure (InvMsg key tag) where
    msgLenLimit = Limit Const.maxInvSize

instance MessageLimitedPure (ReqMsg key tag) where
    msgLenLimit = Limit Const.maxReqSize
