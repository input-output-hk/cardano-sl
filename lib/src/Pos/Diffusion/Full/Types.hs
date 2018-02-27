{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

-- Copy of the "work mode" motif but for the diffusion layer. Collects all the
-- constraints that we expect on a monad capable of carrying the diffusion
-- layer.

module Pos.Diffusion.Full.Types
       ( DiffusionWorkMode
       ) where

import           Universum

import           Mockable (LowLevelAsync, Mockable, MonadMockable)
import qualified Mockable.Metrics as Mockable
import qualified System.Metrics.Counter as Metrics
import qualified System.Metrics.Distribution as Metrics
import qualified System.Metrics.Gauge as Metrics
import           System.Wlog (WithLogger)

type DiffusionWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , Mockable LowLevelAsync m
      , MonadIO m
      , Mockable Mockable.Metrics m
      , Mockable.Distribution m ~ Metrics.Distribution
      , Mockable.Gauge m ~ Metrics.Gauge
      , Mockable.Counter m ~ Metrics.Counter
      )
