{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Base part of LRC consumers.

module Pos.Lrc.ConsumerB
       ( LrcConsumer (..),
         lrcConsumerFromComponent
       ) where

import           Universum

import           Pos.Core.Types (Coin, EpochIndex)
import           Pos.Lrc.Class  (RichmenComponent, rcConsiderDelegated, rcThreshold)
import           Pos.Lrc.Types  (RichmenStake)

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


