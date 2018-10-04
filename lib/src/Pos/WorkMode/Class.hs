{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

{-| The 'WorkMode' constraint, which is widely used throughout the codebase.
    It is a simple alias for a bunch of other useful constraints.
-}

module Pos.WorkMode.Class
       ( WorkMode
       , MinWorkMode
       ) where

import           Universum

import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Random as Rand
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Block (HasBlockConfiguration, HasSlogContext,
                     HasSlogGState, MonadLastKnownHeader)
import           Pos.Chain.Delegation (HasDlgConfiguration, MonadDelegation)
import           Pos.Chain.Security (SecurityParams)
import           Pos.Chain.Ssc (HasSscConfiguration, MonadSscMem)
import           Pos.Chain.Update (HasUpdateConfiguration, UpdateParams)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Context (BlockRetrievalQueue, BlockRetrievalQueueTag,
                     HasSscContext, StartTime, TxpGlobalSettings)
import           Pos.Core (HasPrimaryKey)
import           Pos.Core.JsonLog (CanJsonLog)
import           Pos.Core.Reporting (HasMisbehaviorMetrics, MonadReporting)
import           Pos.DB.Block (MonadBListener)
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.DB.Lrc (HasLrcContext)
import           Pos.DB.Rocks (MonadRealDB)
import           Pos.DB.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem)
import           Pos.DB.Update (UpdateContext)
import           Pos.Infra.DHT.Real.Param (KademliaParams)
import           Pos.Infra.Network.Types (HasNodeType, NetworkConfig)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting.Class (MonadSlots)
import           Pos.Infra.StateLock (StateLock, StateLockMetrics)
import           Pos.Infra.Util.JsonLog.Events (MemPoolModifyReason)
import           Pos.Recovery.Types (MonadRecoveryHeader)
import           Pos.Util (HasLens, HasLens')
import           Pos.Util.Wlog (WithLogger)

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ctx m
    = ( MinWorkMode m
      , MonadBaseControl IO m
      , Rand.MonadRandom m
      , MonadMask m
      , MonadSlots ctx m
      , MonadDB m
      , MonadRealDB ctx m
      , MonadGState m
      , MonadTxpLocal m
      , MonadTxpMem (MempoolExt m) ctx m
      , MonadDelegation ctx m
      , MonadSscMem ctx m
      , MonadRecoveryInfo ctx m
      , MonadRecoveryHeader ctx m
      , MonadLastKnownHeader ctx m
      , MonadBListener m
      , MonadReporting m
      , MonadReader ctx m
      , HasLens' ctx StartTime
      , HasLens' ctx StateLock
      , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
      , HasLens' ctx UpdateContext
      , HasLens' ctx UpdateParams
      , HasLens' ctx SecurityParams
      , HasLens' ctx TxpGlobalSettings
      , HasLens' ctx (NetworkConfig KademliaParams)
      , HasLens BlockRetrievalQueueTag ctx BlockRetrievalQueue
      , HasLrcContext ctx
      , HasSscContext ctx
      , HasMisbehaviorMetrics ctx
      , HasPrimaryKey ctx
      , HasShutdownContext ctx
      , HasSlogContext ctx
      , HasSlogGState ctx
      , HasNodeType ctx
      , HasSscConfiguration
      , HasDlgConfiguration
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadIO m
      , MonadUnliftIO m
      , HasUpdateConfiguration
      , HasNodeConfiguration
      , HasBlockConfiguration
      )
