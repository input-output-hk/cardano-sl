{-# LANGUAGE CPP #-}

{-| 'WorkMode' constraint. It is widely used in almost every our code.
    Simple alias for bunch of useful constraints. This module also
    contains new monads to extend functional capabilities inside do-block.
-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , ProductionMode
       , RawRealMode
       , ServiceMode
       , StatsMode
       ) where


import           Control.Monad.Catch         (MonadMask)
import           Mockable                    (MonadMockable)
import           Mockable.Production         (Production)
import           System.Wlog                 (LoggerNameBox (..), WithLogger)
import           Universum

import qualified Ether
import           Pos.Communication.PeerState (PeerStateHolder, WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.Context                 (ContextHolder, NodeParams, WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.DB.DB                   ()
import           Pos.DB.Holder               (DBHolder)
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Delegation.Holder       (DelegationT)
import           Pos.Lrc.Context             (LrcContext)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Toil       (ExplorerExtra)
#endif
import           Pos.Reporting               (MonadReportingMem)
import           Pos.Shutdown                (MonadShutdownMem)
import           Pos.Slotting.Class          (MonadSlots)
import           Pos.Slotting.MemState       (SlottingHolder)
import           Pos.Slotting.Ntp            (NtpSlotting)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscGStateClass)
import           Pos.Ssc.Extra               (MonadSscMem, SscHolder)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.MemState            (MonadTxpMem, TxpHolder)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.JsonLog            (MonadJL (..))

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
      , MonadMask m
      , MonadSlots m
      , MonadDB m
      , MonadDBLimits m
      , MonadTxpMem TxpExtra_TMP m
      , MonadRelayMem m
      , MonadDelegation m
      , MonadSscMem ssc m
      , MonadReportingMem m
      , SscGStateClass ssc
      , SscLocalDataClass ssc
      , SscHelpersClass ssc
      , WithNodeContext ssc m
      , Ether.MonadReader' LrcContext m
      , Ether.MonadReader' UpdateContext m
      , Ether.MonadReader' NodeParams m
      , Ether.MonadReader' UpdateParams m
      , MonadStats m
      , MonadJL m
      , WithPeerState m
      , MonadShutdownMem m
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , WithPeerState m
      )

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    PeerStateHolder (
    DelegationT (
    TxpHolder TxpExtra_TMP (
    SscHolder ssc (
    NtpSlotting (
    SlottingHolder (
    DBHolder (
    ContextHolder ssc (
    LoggerNameBox Production
    ))))))))

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode = PeerStateHolder (LoggerNameBox Production)
