{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Consumer of data computed by LRC.

module Pos.Lrc.Consumer
       ( LrcConsumer (..)
       , lrcConsumerFromComponent
       , lrcConsumerFromComponentSimple
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
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
    :: RichmenComponent c
    -> (Coin -> m Coin)
    -> (EpochIndex -> m Bool)
    -> (EpochIndex -> Coin -> RichmenStakes -> m ())
    -> LrcConsumer m
lrcConsumerFromComponent rc thd ifNeedCompute callback =
    LrcConsumer
    { lcThreshold = thd
    , lcIfNeedCompute = ifNeedCompute
    , lcComputedCallback = callback
    , lcConsiderDelegated = rcConsiderDelegated rc
    }

-- | Create simple LrcConsumer using constants from RichmenComponent
-- which uses only LRC DB.
lrcConsumerFromComponentSimple
    :: (Bi richmenData, MonadGState m, MonadDB m)
    => RichmenComponent richmenData
    -> (BlockVersionData -> CoinPortion)
    -> LrcConsumer m
lrcConsumerFromComponentSimple rc thresholdGetter =
    lrcConsumerFromComponent rc toThreshold ifNeedCompute onComputed
  where
    toThreshold total =
        flip applyCoinPortionUp total . thresholdGetter <$> gsAdoptedBVData
    ifNeedCompute epoch = isNothing <$> getRichmen rc epoch
    onComputed epoch totalStake stakes =
        putRichmen rc epoch (totalStake, stakes)
