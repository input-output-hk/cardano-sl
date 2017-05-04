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


import           Universum

import           Data.Tagged                  (Tagged)
import qualified Ether
import           Mockable.Production          (Production)
import           System.Wlog                  (LoggerNameBox (..))

import           Pos.Communication.PeerState  (PeerStateHolder)
import           Pos.Context                  (ContextHolder)
import           Pos.DB                       (NodeDBs)
import           Pos.DB.DB                    (DbCoreRedirect)
import           Pos.Delegation.Holder        (DelegationT)
import           Pos.Discovery.Holders        (DiscoveryConstT, DiscoveryKademliaT)
import           Pos.Slotting.MemState        (SlottingVar)
import           Pos.Slotting.MemState.Holder (SlotsDataRedirect)
import           Pos.Slotting.Ntp             (NtpSlottingVar, SlotsRedirect)
import           Pos.Ssc.Extra                (SscHolder)
import           Pos.Statistics.MonadStats    (NoStatsT, StatsT)
import           Pos.Txp.MemState             (TxpHolder)
import           Pos.Update.DB                (DbLimitsRedirect)
import           Pos.Wallet.KeyStorage        (KeyStorageRedirect)
import           Pos.Wallet.WalletMode        (BlockchainInfoRedirect, UpdatesRedirect)
import           Pos.WorkMode.Class           (MinWorkMode, TxpExtra_TMP, WorkMode)


----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    BlockchainInfoRedirect (
    UpdatesRedirect (
    KeyStorageRedirect (
    DbCoreRedirect (
    DbLimitsRedirect (
    PeerStateHolder (
    DelegationT (
    TxpHolder TxpExtra_TMP (
    SscHolder ssc (
    SlotsRedirect (
    SlotsDataRedirect (
    Ether.ReadersT
      ( Tagged NodeDBs NodeDBs
      , Tagged SlottingVar SlottingVar
      , Tagged (Bool, NtpSlottingVar) (Bool, NtpSlottingVar)
      ) (
    ContextHolder ssc (
    LoggerNameBox Production
    )))))))))))))

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
