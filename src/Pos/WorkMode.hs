{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PolyKinds           #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RawRealModeK
       , RawRealModeS
       , ProductionMode
       , RawRealMode(..)
       , ServiceMode(..)
       , StatsMode
       , StaticMode
       ) where


import           Universum

import           Control.Monad.Fix
import           Control.Monad.Trans.Identity   (IdentityT (..))
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Data.Coerce
import           Data.Tagged                    (Tagged)
import qualified Ether
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..), Promise,
                                                 SharedAtomicT, SharedExclusiveT,
                                                 ThreadId)
import           Mockable.Production            (Production)
import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Client.Txp.Balances        (BalancesRedirect)
import           Pos.Client.Txp.History         (TxHistoryRedirect)
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Context                    (NodeContext)
import           Pos.DB                         (DBPureRedirect, MonadGState, NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect, MonadBlockDBWrite)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..), MonadDBRead, MonadDB)
import           Pos.DB.DB                      (GStateCoreRedirect)
import           Pos.Delegation.Class           (DelegationVar)
import           Pos.Discovery.Holders          (DiscoveryConstT, DiscoveryKademliaT)
import           Pos.Slotting.Class             (MonadSlots)
import           Pos.Slotting.MemState          (MonadSlotsData, SlottingVar)
import           Pos.Slotting.MemState.Holder   (SlotsDataRedirect)
import           Pos.Slotting.Ntp               (NtpSlottingVar, SlotsRedirect)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState)
import           Pos.Statistics.MonadStats      (NoStatsT, StatsT)
import           Pos.Txp.MemState               (GenericTxpLocalData, TxpHolderTag)
import           Pos.Types                      (HeaderHash)
import           Pos.Util.Util                  (PowerLift (..))
import           Pos.Util.TimeWarp              (JsonLogT, CanJsonLog (..))
import           Pos.Wallet.WalletMode          (BlockchainInfoRedirect, MonadBalances,
                                                 MonadBlockchainInfo, MonadTxHistory,
                                                 MonadUpdates, UpdatesRedirect)
import           Pos.WorkMode.Class             (MinWorkMode, TxpExtra_TMP, WorkMode)
import           System.Wlog                    (CanLog, HasLoggerName,
                                                 LoggerNameBox (..))

----------------------------------------------------------------------------
-- Concrete types
----------------------------------------------------------------------------

-- | RawRealMode is a basis for `WorkMode`s used to really run system.
type RawRealMode' ssc =
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
        , Tagged DelegationVar DelegationVar
        , Tagged PeerStateTag (PeerStateCtx Production)
        ) (
    Ether.ReadersT (NodeContext ssc) (
    LoggerNameBox (
    JsonLogT
    Production
    ))))))))))))))

newtype RawRealMode ssc a = RawRealMode (RawRealMode' ssc a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFix
    )
type instance ThreadId (RawRealMode ssc) = ThreadId Production
type instance Promise (RawRealMode ssc) = Promise Production
type instance SharedAtomicT (RawRealMode ssc) = SharedAtomicT Production
type instance SharedExclusiveT (RawRealMode ssc) = SharedExclusiveT Production
type instance Gauge (RawRealMode ssc) = Gauge Production
type instance ChannelT (RawRealMode ssc) = ChannelT Production
type instance Distribution (RawRealMode ssc) = Distribution Production
type instance Counter (RawRealMode ssc) = Counter Production

deriving instance CanLog (RawRealMode ssc)
deriving instance HasLoggerName (RawRealMode ssc)
deriving instance MonadSlotsData (RawRealMode ssc)
deriving instance MonadSlots (RawRealMode ssc)
deriving instance MonadGState (RawRealMode ssc)
deriving instance MonadDBRead (RawRealMode ssc)
deriving instance MonadDB (RawRealMode ssc)
deriving instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RawRealMode ssc)
deriving instance MonadBListener (RawRealMode ssc)
deriving instance MonadUpdates (RawRealMode ssc)
deriving instance SscHelpersClass ssc => MonadBlockchainInfo (RawRealMode ssc)
deriving instance MonadBalances (RawRealMode ssc)
deriving instance MonadTxHistory (RawRealMode ssc)
deriving instance WithPeerState (RawRealMode ssc)
deriving instance CanJsonLog (RawRealMode ssc)

instance PowerLift m (RawRealMode' ssc) => PowerLift m (RawRealMode ssc) where
  powerLift = RawRealMode . powerLift

instance
    MonadBlockDBGeneric header blk undo (RawRealMode' ssc) =>
    MonadBlockDBGeneric header blk undo (RawRealMode ssc) where
    dbGetHeader = (coerce :: (HeaderHash -> RawRealMode' ssc (Maybe header)) ->
                             (HeaderHash -> RawRealMode ssc (Maybe header)))
                  (dbGetHeader @header @blk @undo)
    dbGetBlock = (coerce :: (HeaderHash -> RawRealMode' ssc (Maybe blk)) ->
                            (HeaderHash -> RawRealMode ssc (Maybe blk)))
                 (dbGetBlock @header @blk @undo)
    dbGetUndo = (coerce :: (HeaderHash -> RawRealMode' ssc (Maybe undo)) ->
                           (HeaderHash -> RawRealMode ssc (Maybe undo)))
                 (dbGetUndo @header @blk @undo)

instance
    ( Mockable d (RawRealMode' ssc)
    , MFunctor' d (RawRealMode ssc) (RawRealMode' ssc)
    )
    => Mockable d (RawRealMode ssc) where
    liftMockable dmt = RawRealMode $ liftMockable $ hoist' (\(RawRealMode m) -> m) dmt

instance
    Ether.MonadReader tag r (RawRealMode' ssc) =>
    Ether.MonadReader tag r (RawRealMode ssc)
  where
    ask =
        (coerce :: RawRealMode' ssc r -> RawRealMode ssc r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (RawRealMode' ssc) a ->
            Lift.Local r (RawRealMode ssc) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> RawRealMode' ssc a) ->
            ((r -> a) -> RawRealMode ssc a))
        (Ether.reader @tag)

-- | RawRealMode + kademlia. Used in wallet too.
type RawRealModeK ssc = DiscoveryKademliaT (RawRealMode ssc)

-- | RawRealMode + static peers.
type RawRealModeS ssc = DiscoveryConstT (RawRealMode ssc)

-- | ProductionMode is an instance of WorkMode which is used
-- (unsurprisingly) in production.
type ProductionMode ssc = NoStatsT $ RawRealModeK ssc

-- | StatsMode is used for remote benchmarking.
type StatsMode ssc = StatsT $ RawRealModeK ssc

-- | Fixed peer discovery without stats.
type StaticMode ssc = NoStatsT $ RawRealModeS ssc

-- | ServiceMode is the mode in which support nodes work.
type ServiceMode' =
    PeerStateRedirect (
    Ether.ReaderT PeerStateTag (PeerStateCtx Production) (
    LoggerNameBox (
    JsonLogT Production
    )))

newtype ServiceMode a = ServiceMode (ServiceMode' a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFix
    )
type instance ThreadId (ServiceMode) = ThreadId Production
type instance Promise (ServiceMode) = Promise Production
type instance SharedAtomicT (ServiceMode) = SharedAtomicT Production
type instance SharedExclusiveT (ServiceMode) = SharedExclusiveT Production
type instance Gauge (ServiceMode) = Gauge Production
type instance ChannelT (ServiceMode) = ChannelT Production
type instance Distribution (ServiceMode) = Distribution Production
type instance Counter (ServiceMode) = Counter Production

deriving instance CanLog (ServiceMode)
deriving instance HasLoggerName (ServiceMode)
deriving instance WithPeerState (ServiceMode)
deriving instance CanJsonLog (ServiceMode)

instance PowerLift m ServiceMode' => PowerLift m (ServiceMode) where
  powerLift = ServiceMode . powerLift

instance
    ( Mockable d (ServiceMode')
    , MFunctor' d (ServiceMode) (ServiceMode')
    )
    => Mockable d (ServiceMode) where
    liftMockable dmt = ServiceMode $ liftMockable $ hoist' (\(ServiceMode m) -> m) dmt

instance
    Ether.MonadReader tag r ServiceMode' =>
    Ether.MonadReader tag r ServiceMode
  where
    ask =
        (coerce :: ServiceMode' r -> ServiceMode r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (ServiceMode') a ->
            Lift.Local r (ServiceMode) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> ServiceMode' a) ->
            ((r -> a) -> ServiceMode a))
        (Ether.reader @tag)
