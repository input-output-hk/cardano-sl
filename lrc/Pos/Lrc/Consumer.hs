{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Consumer of data computed by LRC.

module Pos.Lrc.Consumer
       ( LrcConsumer (..)
       , lrcConsumerFromComponent
       , lrcConsumerFromComponentSimple
       ) where

import           Universum

import           Pos.Core (BlockVersionData, Coin, CoinPortion, EpochIndex, applyCoinPortionUp)
import           Pos.DB.Class (MonadDB, MonadGState, gsAdoptedBVData)
import           Pos.Lrc.DB.RichmenBase (getRichmen, putRichmen)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (RichmenStakes)

-- | Datatype for LRC computation client.
-- If you want to compute richmen, you should add such client to LRC framework
data LrcConsumer m = LrcConsumer
    {
      lcThreshold         :: Coin -> m Coin
    -- ^ Function which computes threshold taking total stake in
    -- the system as an argument.
    , lcIfNeedCompute     :: EpochIndex -> m Bool
    -- ^ Function which defines necessity of richmen computation
    , lcComputedCallback  :: EpochIndex -> Coin -> RichmenStakes -> m ()
    -- ^ Callback which will be called when richmen computed
    , lcConsiderDelegated :: Bool
    -- ^ Whether delegated stake should be considered
    }

-- | Create LrcConsumer using constants from RichmenComponent and
-- given actions.
lrcConsumerFromComponent
    :: forall c m.
       RichmenComponent c
    => (Coin -> m Coin)
    -> (EpochIndex -> m Bool)
    -> (EpochIndex -> Coin -> RichmenStakes -> m ())
    -> LrcConsumer m
lrcConsumerFromComponent thd ifNeedCompute callback =
    LrcConsumer
    { lcThreshold = thd
    , lcIfNeedCompute = ifNeedCompute
    , lcComputedCallback = callback
    , lcConsiderDelegated = rcConsiderDelegated $ Proxy @c
    }

-- | Create simple LrcConsumer using constants from RichmenComponent
-- which uses only LRC DB.
lrcConsumerFromComponentSimple
    :: forall c m.
       (RichmenComponent c, MonadGState m, MonadDB m)
    => (BlockVersionData -> CoinPortion) -> LrcConsumer m
lrcConsumerFromComponentSimple thresholdGetter =
    lrcConsumerFromComponent @c toThreshold ifNeedCompute onComputed
  where
    toThreshold total =
        flip applyCoinPortionUp total . thresholdGetter <$> gsAdoptedBVData
    ifNeedCompute epoch = isNothing <$> getRichmen @c epoch
    onComputed epoch totalStake stakes =
        putRichmen @c epoch (totalStake, stakes)
