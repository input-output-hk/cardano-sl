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
import           Mockable (MonadMockable)
import           System.Wlog (WithLogger)

import           Pos.Communication.Limits
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadFormatPeers, MonadKnownPeers)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.TimeWarp (CanJsonLog)

type DiffusionWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadMockable m
      , MonadIO m
      , HasConfiguration
      , HasInfraConfiguration
      , HasUpdateConfiguration
      , HasSscConfiguration
      , HasNodeConfiguration
      , MonadBaseControl IO m
      , Rand.MonadRandom m
      , MonadMask m
      , HasBlockLimits m
      , HasSscLimits m
      , HasTxpLimits m
      , HasUpdateLimits m
      , MonadKnownPeers m
      , MonadFormatPeers m
      )
