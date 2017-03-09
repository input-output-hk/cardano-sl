{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Consumer of data computed by LRC.

module Pos.Lrc.Consumer
       ( LrcConsumer (..)
       , lrcConsumerFromComponent
       , lrcConsumerFromComponentSimple
       ) where

import           Universum

import           Pos.DB.Class       (MonadDB)
import           Pos.Lrc.ConsumerB  (LrcConsumer, lrcConsumerFromComponent)
import           Pos.Lrc.DB.Richmen (RichmenComponent (..), getRichmen, putRichmen)
import           Pos.Lrc.Types      (RichmenStake)
import           Pos.Types          (Coin, EpochIndex)

-- | Create simple LrcConsumer using constants from RichmenComponent
-- which uses only LRC DB.
lrcConsumerFromComponentSimple
    :: forall c m.
       (RichmenComponent c, MonadDB m)
    => LrcConsumer m
lrcConsumerFromComponentSimple =
    lrcConsumerFromComponent @c ifNeedCompute onComputed
  where
    ifNeedCompute epoch = isNothing <$> getRichmen @c epoch
    onComputed epoch totalStake stakes =
        putRichmen @c epoch (totalStake, stakes)
