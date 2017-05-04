{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RawRealModeK
       , ProductionMode
       , RawRealMode
       , ServiceMode
       , StatsMode
       , StaticMode
       ) where


import           Mockable.Production         (Production)
import           System.Wlog                 (LoggerNameBox (..))
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder)
import           Pos.Context                 (ContextHolder)
import           Pos.DB.DB                   ()
import           Pos.DB.Holder               (DBHolder)
import           Pos.Delegation.Holder       (DelegationT)
import           Pos.Discovery.Holders       (DiscoveryConstT, DiscoveryKademliaT)
import           Pos.Slotting.MemState       (SlottingHolder)
import           Pos.Slotting.Ntp            (NtpSlotting)
import           Pos.Ssc.Extra               (SscHolder)
import           Pos.Statistics.MonadStats   (NoStatsT, StatsT)
import           Pos.Txp.MemState            (TxpHolder)
import           Pos.Wallet.KeyStorage       (KeyStorageRedirect)
import           Pos.Wallet.WalletMode       (BlockchainInfoRedirect, UpdatesRedirect)
import           Pos.WorkMode.Class          (MinWorkMode, TxpExtra_TMP, WorkMode)


----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    BlockchainInfoRedirect (
    PeerStateHolder (
    DelegationT (
    TxpHolder TxpExtra_TMP (
    SscHolder ssc (
    NtpSlotting (
    SlottingHolder (
    DBHolder (
    UpdatesRedirect (
    KeyStorageRedirect (
    ContextHolder ssc (
    LoggerNameBox Production
    )))))))))))

-- | RawRealMode + kademlia. Used in wallet too.
type RawRealModeK ssc = DiscoveryKademliaT (RawRealMode ssc)

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT $ RawRealModeK ssc

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT $ RawRealModeK ssc

-- | Fixed peer discovery without stats.
type StaticMode ssc = NoStatsT $ DiscoveryConstT (RawRealMode ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode = PeerStateHolder (LoggerNameBox Production)
