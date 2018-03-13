{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

-- Copy of the "work mode" motif but for the diffusion layer. Collects all the
-- constraints that we expect on a monad capable of carrying the diffusion
-- layer.

module Pos.Diffusion.Full.Types
       ( DiffusionWorkMode
       ) where

import           Universum

import qualified Crypto.Random as Rand
import           Mockable (LowLevelAsync, Mockable, MonadMockable)
import qualified Mockable.Metrics as Mockable
import qualified System.Metrics.Counter as Metrics
import qualified System.Metrics.Distribution as Metrics
import qualified System.Metrics.Gauge as Metrics
import           System.Wlog (WithLogger)

import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Communication.Limits (HasAdoptedBlockVersionData)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)

type DiffusionWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , Mockable LowLevelAsync m
      , MonadIO m
      -- Unfortunately we need HasConfigurations because so much of the core
      -- program depends upon it (serialization, message limits, smart
      -- constructors).
      -- [CSL-2141] aspires to fix that. There's a lot of stuff in here that
      -- a diffusion layer simply should not need to know about.
      , HasConfiguration
      -- Needed for the recoveryHeadersMessage, which is not ideal but whatever
      -- we can deal with that later.
      , HasBlockConfiguration
      -- Needed for message size limits, but shouldn't be [CSL-2242].
      , HasNodeConfiguration
      , Rand.MonadRandom m
      , MonadMask m
      -- TODO should not need HasAdoptedBlockVersionData
      --
      -- It's a stop-gap to provide message limits.
      -- Really though, these limits should not *necessarily* come from block
      -- version data; we ought to be able to drop in our own in case of, for
      -- example, a simulation or test.
      --
      -- The plan: define message limits at value-level rather than via
      -- typeclass, and take a record of (mutable) limits as a parameter to the
      -- diffusion layer.
      , HasAdoptedBlockVersionData m
      , Mockable Mockable.Metrics m
      , Mockable.Distribution m ~ Metrics.Distribution
      , Mockable.Gauge m ~ Metrics.Gauge
      , Mockable.Counter m ~ Metrics.Counter
      )
