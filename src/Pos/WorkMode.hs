{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RawRealModeK
       , RawRealModeS
       , ProductionMode
       , StatsMode
       , StaticMode
       , RunModeHolder (..)
       , RawRealMode
       , ServiceMode
       ) where


import           Universum

import           Control.Lens                   (iso)
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Data.Coerce                    (coerce)
import           Data.Tagged                    (Tagged)
import qualified Ether
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor', Mockable (..), Promise,
                                                 SharedAtomicT, SharedExclusiveT,
                                                 ThreadId, liftMockableWrappedM)
import           Mockable.Production            (Production)
import           Serokell.Util.Lens             (WrappedM (..))
import           System.Wlog                    (CanLog, HasLoggerName,
                                                 LoggerNameBox (..))

import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Client.Txp.Balances        (BalancesRedirect)
import           Pos.Client.Txp.History         (TxHistoryRedirect)
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Context                    (NodeContext)
import           Pos.DB                         (DBPureRedirect, MonadGStateCore, NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..), MonadDBPure)
import           Pos.DB.DB                      (GStateCoreRedirect)
import           Pos.Delegation.Class           (DelegationWrap)
import           Pos.Discovery.Class            (MonadDiscovery)
import           Pos.Discovery.Holders          (DiscoveryConstT, DiscoveryKademliaT)
import           Pos.Slotting.Class             (MonadSlots)
import           Pos.Slotting.MemState          (MonadSlotsData, SlotsDataRedirect,
                                                 SlottingVar)
import           Pos.Slotting.Ntp               (NtpSlottingVar, SlotsRedirect)
import           Pos.Ssc.Extra                  (SscMemTag, SscState)
import           Pos.Statistics.MonadStats      (MonadStats, NoStatsT, StatsT)
import           Pos.Txp.MemState               (GenericTxpLocalData, TxpHolderTag)
import           Pos.Types                      (HeaderHash)
import           Pos.Wallet.WalletMode          (BlockchainInfoRedirect, UpdatesRedirect)
import           Pos.WorkMode.Class             (MinWorkMode, TxpExtra_TMP, WorkMode)

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode ssc =
    BListenerStub (
    BlockchainInfoRedirect (
    UpdatesRedirect (
    GStateCoreRedirect (
    PeerStateRedirect (
    TxHistoryRedirect (
    BalancesRedirect (
    SlotsRedirect (
    SlotsDataRedirect (
    BlockDBRedirect (
    DBPureRedirect (
    Ether.ReadersT
        ( Tagged NodeDBs NodeDBs
        , Tagged SlottingVar SlottingVar
        , Tagged (Bool, NtpSlottingVar) (Bool, NtpSlottingVar)
        , Tagged SscMemTag (SscState ssc)
        , Tagged TxpHolderTag (GenericTxpLocalData TxpExtra_TMP)
        , Tagged (TVar DelegationWrap) (TVar DelegationWrap)
        , Tagged PeerStateTag (PeerStateCtx Production)
        ) (
    Ether.ReadersT (NodeContext ssc) (
    LoggerNameBox Production
    )))))))))))))

-- | RawRealMode + kademlia. Used in wallet too.
type RawRealModeK ssc = DiscoveryKademliaT (RawRealMode ssc)

-- | RawRealMode + static peers.
type RawRealModeS ssc = DiscoveryConstT (RawRealMode ssc)

----------------------------------------------------------------------------
-- ProductionMode
----------------------------------------------------------------------------

newtype RunModeHolder m a
    = RunModeHolder
    { getRunModeHolder :: m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , CanLog
    , HasLoggerName
    , MonadSlotsData
    , MonadSlots
    , MonadGStateCore
    , MonadDBPure
    , MonadBListener
    , MonadDiscovery
    , MonadStats
    , WithPeerState
    )

instance
    MonadBlockDBGeneric header blk undo m =>
    MonadBlockDBGeneric header blk undo (RunModeHolder m) where
    dbGetHeader = (coerce :: (HeaderHash -> m (Maybe header)) ->
                             (HeaderHash -> RunModeHolder m (Maybe header)))
                  (dbGetHeader @header @blk @undo)
    dbGetBlock = (coerce :: (HeaderHash -> m (Maybe blk)) ->
                            (HeaderHash -> RunModeHolder m (Maybe blk)))
                 (dbGetBlock @header @blk @undo)
    dbGetUndo = (coerce :: (HeaderHash -> m (Maybe undo)) ->
                           (HeaderHash -> RunModeHolder m (Maybe undo)))
                 (dbGetUndo @header @blk @undo)

type instance ThreadId (RunModeHolder m) = ThreadId m
type instance Promise (RunModeHolder m) = Promise m
type instance SharedAtomicT (RunModeHolder m) = SharedAtomicT m
type instance SharedExclusiveT (RunModeHolder m) = SharedExclusiveT m
type instance Gauge (RunModeHolder m) = Gauge m
type instance ChannelT (RunModeHolder m) = ChannelT m
type instance Distribution (RunModeHolder m) = Distribution m
type instance Counter (RunModeHolder m) = Counter m

instance Monad m => WrappedM (RunModeHolder m) where
    type UnwrappedM (RunModeHolder m) = m
    _WrappedM = iso getRunModeHolder RunModeHolder

instance
    ( Mockable d m
    , MFunctor' d (RunModeHolder m) m)
    => Mockable d (RunModeHolder m) where
    liftMockable = liftMockableWrappedM

instance
    Ether.MonadReader tag r m =>
    Ether.MonadReader tag r (RunModeHolder m)
  where
    ask =
        (coerce :: m r -> RunModeHolder m r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r m a ->
            Lift.Local r (RunModeHolder m) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> m a) ->
            ((r -> a) -> RunModeHolder m a))
        (Ether.reader @tag)

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionModeRaw ssc = NoStatsT (RawRealModeK ssc)
type ProductionMode ssc = RunModeHolder (ProductionModeRaw ssc)

-- | StatsMode is used for remote benchmarking.
type StatsModeRaw ssc = StatsT $ RawRealModeK ssc
type StatsMode ssc = RunModeHolder (StatsModeRaw ssc)

-- | Fixed peer discovery without stats.
type StaticModeRaw ssc = NoStatsT (RawRealModeS ssc)
type StaticMode ssc = RunModeHolder (StaticModeRaw ssc)

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode =
    PeerStateRedirect (
    Ether.ReaderT PeerStateTag (PeerStateCtx Production) (
    LoggerNameBox Production
    ))
