{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

{-| 'WorkMode' constraint. It is widely used in almost every our code.
    Simple alias for bunch of useful constraints. This module also
    contains new monads to extend functional capabilities inside do-block.
-}

module Pos.WorkMode.Class
       ( WorkMode
       , MinWorkMode
       , TxpExtra_TMP
       ) where

import           Universum

import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Random               as Rand
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Block.BListener         (MonadBListener)
import           Pos.Block.Slog.Types        (HasSlogContext, HasSlogGState)
import           Pos.Context                 (BlockRetrievalQueue, BlockRetrievalQueueTag,
                                              HasSscContext, MonadLastKnownHeader,
                                              MonadProgressHeader, MonadRecoveryHeader,
                                              StartTime, TxpGlobalSettings)
import           Pos.DB.Block                (MonadBlockDBWrite, MonadSscBlockDB)
import           Pos.DB.Class                (MonadDB, MonadGState)
import           Pos.DB.Rocks                (MonadRealDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.DHT.Real.Types          (KademliaDHTInstance)
import           Pos.Lrc.Context             (LrcContext)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Toil       (ExplorerExtra)
#endif
import           Pos.Configuration           (HasNodeConfiguration)
import           Pos.Core                    (HasConfiguration, HasPrimaryKey)
import           Pos.Infra.Configuration     (HasInfraConfiguration)
import           Pos.KnownPeers              (MonadFormatPeers, MonadKnownPeers)
import           Pos.Network.Types           (NetworkConfig)
import           Pos.Recovery.Info           (MonadRecoveryInfo)
import           Pos.Reporting               (HasReportingContext)
import           Pos.Security.Params         (SecurityParams)
import           Pos.Shutdown                (HasShutdownContext)
import           Pos.Slotting.Class          (MonadSlots)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscGStateClass)
import           Pos.Ssc.Class.Workers       (SscWorkersClass)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.StateLock               (StateLock, StateLockMetrics)
import           Pos.Txp.MemState            (MonadTxpMem)
import           Pos.Update.Configuration    (HasUpdateConfiguration)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.TimeWarp           (CanJsonLog)
import           Pos.Util.Util               (HasLens, HasLens')

-- Something extremely unpleasant.
-- TODO: get rid of it after CSL-777 is done.
#ifdef WITH_EXPLORER
type TxpExtra_TMP = ExplorerExtra
#else
type TxpExtra_TMP = ()
#endif

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc ctx m
    = ( MinWorkMode m
      , MonadBaseControl IO m
      , Rand.MonadRandom m
      , MonadMask m
      , MonadSlots ctx m
      , MonadDB m
      , MonadRealDB ctx m
      , MonadGState m
      , MonadSscBlockDB ssc m
      , MonadBlockDBWrite ssc m
      , MonadTxpMem TxpExtra_TMP ctx m
      , MonadDelegation ctx m
      , MonadSscMem ssc ctx m
      , SscGStateClass ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , SscWorkersClass ssc
      , MonadRecoveryInfo m
      , MonadRecoveryHeader ssc ctx m
      , MonadProgressHeader ssc ctx m
      , MonadLastKnownHeader ssc ctx m
      , MonadBListener m
      , MonadReader ctx m
      , MonadKnownPeers m
      , MonadFormatPeers m
      , HasLens StartTime ctx StartTime
      , HasLens' ctx StateLock
      , HasLens' ctx StateLockMetrics
      , HasLens LrcContext ctx LrcContext
      , HasLens UpdateContext ctx UpdateContext
      , HasLens UpdateParams ctx UpdateParams
      , HasLens SecurityParams ctx SecurityParams
      , HasLens TxpGlobalSettings ctx TxpGlobalSettings
      , HasLens BlockRetrievalQueueTag ctx (BlockRetrievalQueue ssc)
      , HasLens (NetworkConfig KademliaDHTInstance) ctx (NetworkConfig KademliaDHTInstance)
      , HasSscContext ssc ctx
      , HasReportingContext ctx
      , HasPrimaryKey ctx
      , HasShutdownContext ctx
      , HasSlogContext ctx
      , HasSlogGState ctx
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadMockable m
      , MonadIO m
      , HasConfiguration
      , HasInfraConfiguration
      , HasUpdateConfiguration
      , HasNodeConfiguration
      )
