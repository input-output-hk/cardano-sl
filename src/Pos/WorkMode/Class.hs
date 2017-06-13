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

import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Ether
import           Mockable                    (MonadMockable)
import           Universum                   (IO)
import           System.Wlog                 (WithLogger)

import           Pos.Block.BListener         (MonadBListener)
import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.Context                 (BlkSemaphore, MonadBlockRetrievalQueue,
                                              MonadLastKnownHeader, MonadProgressHeader,
                                              MonadRecoveryHeader, MonadSscContext,
                                              NodeParams, StartTime, TxpGlobalSettings)
import           Pos.DB.Block                (MonadBlockDBWrite)
import           Pos.DB.Class                (MonadDB, MonadGState, MonadRealDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Discovery.Class         (MonadDiscovery)
import           Pos.Lrc.Context             (LrcContext)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Toil       (ExplorerExtra)
#endif
import           Pos.Core                    (MonadPrimaryKey)
import           Pos.Recovery.Info           (MonadRecoveryInfo)
import           Pos.Reporting               (MonadReportingMem)
import           Pos.Security.Params         (SecurityParams)
import           Pos.Shutdown                (MonadShutdownMem)
import           Pos.Slotting.Class          (MonadSlots)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscGStateClass)
import           Pos.Ssc.Extra               (MonadSscMem)
import           Pos.Txp.MemState            (MonadTxpMem)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.TimeWarp           (CanJsonLog)

-- Something extremely unpleasant.
-- TODO: get rid of it after CSL-777 is done.
#ifdef WITH_EXPLORER
type TxpExtra_TMP = ExplorerExtra
#else
type TxpExtra_TMP = ()
#endif

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ssc m
    = ( MinWorkMode m
      , MonadBaseControl IO m
      , MonadMask m
      , MonadSlots m
      , MonadDB m
      , MonadBlockDBWrite ssc m
      , MonadGState m
      , MonadRealDB m
      , MonadTxpMem TxpExtra_TMP m
      , MonadRelayMem m
      , MonadDelegation m
      , MonadSscMem ssc m
      , MonadReportingMem m
      , SscGStateClass ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , MonadBlockRetrievalQueue ssc m
      , MonadRecoveryInfo m
      , MonadRecoveryHeader ssc m
      , MonadProgressHeader ssc m
      , MonadLastKnownHeader ssc m
      , MonadPrimaryKey m
      , Ether.MonadReader' StartTime m
      , Ether.MonadReader' BlkSemaphore m
      , Ether.MonadReader' LrcContext m
      , Ether.MonadReader' UpdateContext m
      , Ether.MonadReader' NodeParams m
      , Ether.MonadReader' UpdateParams m
      , Ether.MonadReader' SecurityParams m
      , Ether.MonadReader' TxpGlobalSettings m
      , MonadSscContext ssc m
      , WithPeerState m
      , MonadShutdownMem m
      , MonadBListener m
      , MonadDiscovery m
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      )
