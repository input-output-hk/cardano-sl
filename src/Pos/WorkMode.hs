{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

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

import           Pos.Communication.PeerState (PeerStateHolder (..), WithPeerState)
import           Pos.Communication.Relay     (MonadRelayMem)
import           Pos.Context                 (ContextHolder, NodeParams, WithNodeContext)
import           Pos.DB.Class                (MonadDB, MonadDBCore)
import           Pos.DB.DB                   ()
import           Pos.DB.Holder               (DBHolder)
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Delegation.Holder       (DelegationT (..))
import           Pos.Lrc.Context             (LrcContext)
#ifdef WITH_EXPLORER
import           Pos.Explorer.Txp.Toil       (ExplorerExtra)
#endif
import           Pos.Reporting               (MonadReportingMem)
import           Pos.Shutdown                (MonadShutdownMem)
import           Pos.Slotting.Class          (MonadSlots)
import           Pos.Slotting.MemState       (MonadSlotsData, SlottingHolder (..))
import           Pos.Slotting.Ntp            (NtpSlotting (..))
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass)
import           Pos.Ssc.Class.Storage       (SscGStateClass)
import           Pos.Ssc.Extra               (MonadSscMem, SscHolder)
import           Pos.Statistics.MonadStats   (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.MemState            (MonadTxpMem, TxpHolder)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Update.Params           (UpdateParams)
import           Pos.Util.Context            (HasContext)
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
      , HasContext LrcContext m
      , HasContext UpdateContext m
      , HasContext NodeParams m
      , HasContext UpdateParams m
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

-- Maybe we should move to somewhere else
deriving instance (Monad m, WithNodeContext ssc m) => WithNodeContext ssc (PeerStateHolder m)
deriving instance WithNodeContext ssc m => WithNodeContext ssc (NtpSlotting m)
deriving instance WithNodeContext ssc m => WithNodeContext ssc (SlottingHolder m)

deriving instance MonadSlots m => MonadSlots (PeerStateHolder m)
deriving instance MonadSlotsData m => MonadSlotsData (PeerStateHolder m)

deriving instance MonadDB m => MonadDB (PeerStateHolder m)
deriving instance MonadDBCore m => MonadDBCore (PeerStateHolder m)
deriving instance MonadDBLimits m => MonadDBLimits (PeerStateHolder m)
deriving instance MonadDB m => MonadDB (NtpSlotting m)
deriving instance MonadDBCore m => MonadDBCore (NtpSlotting m)
deriving instance MonadDBLimits m => MonadDBLimits (NtpSlotting m)
deriving instance MonadDB m => MonadDB (SlottingHolder m)
deriving instance MonadDBCore m => MonadDBCore (SlottingHolder m)
deriving instance MonadDBLimits m => MonadDBLimits (SlottingHolder m)

deriving instance MonadDelegation m => MonadDelegation (PeerStateHolder m)

deriving instance MonadReportingMem m => MonadReportingMem (PeerStateHolder m)
deriving instance MonadReportingMem m => MonadReportingMem (NtpSlotting m)
deriving instance MonadReportingMem m => MonadReportingMem (SlottingHolder m)

deriving instance MonadRelayMem m => MonadRelayMem (PeerStateHolder m)
deriving instance MonadRelayMem m => MonadRelayMem (NtpSlotting m)
deriving instance MonadRelayMem m => MonadRelayMem (SlottingHolder m)

deriving instance MonadSscMem ssc m => MonadSscMem ssc (PeerStateHolder m)
deriving instance MonadTxpMem x m => MonadTxpMem x (PeerStateHolder m)

deriving instance MonadShutdownMem m => MonadShutdownMem (PeerStateHolder m)
deriving instance MonadShutdownMem m => MonadShutdownMem (NtpSlotting m)
deriving instance MonadShutdownMem m => MonadShutdownMem (SlottingHolder m)

deriving instance MonadJL m => MonadJL (PeerStateHolder m)
deriving instance MonadJL m => MonadJL (NtpSlotting m)
deriving instance MonadJL m => MonadJL (SlottingHolder m)

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    PeerStateHolder (
    DelegationT (
    TxpHolder TxpExtra_TMP (
    SscHolder ssc (
    NtpSlotting (
    SlottingHolder (
    ContextHolder ssc (
    DBHolder (
    LoggerNameBox Production
    ))))))))

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT (RawRealMode ssc)

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode = PeerStateHolder (LoggerNameBox Production)
