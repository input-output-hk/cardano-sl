{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Consumer of data computed by LRC.

module Pos.Lrc.Consumer
       ( LrcConsumer (..)
       , lrcConsumerFromComponent
       , lrcConsumerFromComponentSimple
       ) where

import           Universum

import           Pos.DB.Class       (MonadDB)
import           Pos.DB.Lrc.Richmen (RichmenComponent (..), getRichmen, putRichmen)
import           Pos.Lrc.Types      (RichmenStake)
import           Pos.Types          (Coin, EpochIndex)

-- | Datatype for LRC computation client.
-- If you want to compute richmen, you should add such client to LRC framework
data LrcConsumer m = LrcConsumer
    {
      lcThreshold         :: Coin -> Coin
    -- ^ Function which defines threshold depends on total stake
    , lcIfNeedCompute     :: EpochIndex -> m Bool
    -- ^ Function which defines necessity of richmen computation
    , lcComputedCallback  :: EpochIndex -> Coin -> RichmenStake -> m ()
    -- ^ Callback which will be called when richmen computed
    , lcConsiderDelegated :: Bool
    -- ^ Whether delegated stake should be considered
    }

-- | Create LrcConsumer using constants from RichmenComponent and
-- given actions.
lrcConsumerFromComponent
    :: forall c m.
       RichmenComponent c
    => (EpochIndex -> m Bool)
    -> (EpochIndex -> Coin -> RichmenStake -> m ())
    -> LrcConsumer m
lrcConsumerFromComponent ifNeedCompute callback =
    LrcConsumer
    { lcThreshold = rcThreshold proxy
    , lcIfNeedCompute = ifNeedCompute
    , lcComputedCallback = callback
    , lcConsiderDelegated = rcConsiderDelegated proxy
    }
  where
    proxy :: Proxy c
    proxy = Proxy

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
