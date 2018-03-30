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
import           Mockable (MonadMockable)
import           System.Wlog (WithLogger)
import           UnliftIO (MonadUnliftIO)

import           Pos.Block.BListener (MonadBListener)
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Block.Slog (HasSlogContext, HasSlogGState)
import           Pos.Block.Types (MonadLastKnownHeader, MonadRecoveryHeader)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Context (BlockRetrievalQueue, BlockRetrievalQueueTag, HasSscContext, StartTime,
                              TxpGlobalSettings)
import           Pos.Core (HasConfiguration, HasPrimaryKey)
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.DB.Rocks (MonadRealDB)
import           Pos.Delegation.Class (MonadDelegation)
import           Pos.Delegation.Configuration (HasDlgConfiguration)
import           Pos.DHT.Real.Param (KademliaParams)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Network.Types (HasNodeType, NetworkConfig)
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (HasReportingContext, MonadReporting)
import           Pos.Security.Params (SecurityParams)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Ssc.Mem (MonadSscMem)
import           Pos.StateLock (StateLock, StateLockMetrics)
import           Pos.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Update.Context (UpdateContext)
import           Pos.Update.Params (UpdateParams)
import           Pos.Util (HasLens, HasLens')
import           Pos.Util.JsonLog.Events (MemPoolModifyReason)
import           Pos.Util.TimeWarp (CanJsonLog)

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
      , MonadRecoveryInfo m
      , MonadRecoveryHeader ctx m
      , MonadLastKnownHeader ctx m
      , MonadBListener m
      , MonadReporting ctx m
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
      , HasReportingContext ctx
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
      , MonadMockable m
      , MonadIO m
      , MonadUnliftIO m
      , HasConfiguration
      , HasUpdateConfiguration
      , HasNodeConfiguration
      , HasBlockConfiguration
      )
