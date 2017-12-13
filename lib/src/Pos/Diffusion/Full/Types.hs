{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

-- Copy of the "work mode" motif but for the diffusion layer. Collects all the
-- constraints that we expect on a monad capable of carrying the diffusion
-- layer.

module Pos.Diffusion.Full.Types
       ( DiffusionWorkMode
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Random as Rand
import           Mockable (Mockable, MonadMockable)
import qualified Mockable.Metrics as Mockable
import           System.Wlog (WithLogger)
import qualified System.Metrics.Counter as Metrics
import qualified System.Metrics.Distribution as Metrics
import qualified System.Metrics.Gauge as Metrics

import           Pos.Communication.Limits (HasAdoptedBlockVersionData)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.TimeWarp (CanJsonLog)

type DiffusionWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadMockable m
      , MonadIO m
      , HasConfiguration
      , HasBlockConfiguration
      , HasInfraConfiguration
      , HasUpdateConfiguration
      , HasSscConfiguration
      , HasNodeConfiguration
      , MonadBaseControl IO m
      , Rand.MonadRandom m
      , MonadMask m
      , HasAdoptedBlockVersionData m
      , Mockable Mockable.Metrics m
      , Mockable.Distribution m ~ Metrics.Distribution
      , Mockable.Gauge m ~ Metrics.Gauge
      , Mockable.Counter m ~ Metrics.Counter
      )
