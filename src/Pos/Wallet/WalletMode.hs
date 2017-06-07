{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , RawWalletMode(..)
       , WalletMode
       , WalletRealMode
       , WalletStaticPeersMode

       -- * Monadic redirect
       , BalancesWalletRedirect
       , runBalancesWalletRedirect
       , TxHistoryWalletRedirect
       , runTxHistoryWalletRedirect
       , BlockchainInfoNotImplemented
       , runBlockchainInfoNotImplemented
       , BlockchainInfoRedirect
       , runBlockchainInfoRedirect
       , UpdatesNotImplemented
       , runUpdatesNotImplemented
       , UpdatesRedirect
       , runUpdatesRedirect
       ) where

import           Universum

import           Control.Concurrent.STM         (tryReadTMVar)
import           Control.Monad.Fix
import           Control.Monad.Trans            (MonadTrans)
import           Control.Monad.Trans.Identity   (IdentityT (..))
import qualified Control.Monad.Trans.Lift.Local as Lift
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Data.Coerce                    (coerce)
import           Data.Tagged                    (Tagged (..))
import           Data.Time.Units                (Millisecond)
import qualified Ether
import           Mockable                       (ChannelT, Counter, Distribution, Gauge,
                                                 MFunctor' (..), Mockable (..),
                                                 Production, Promise, SharedAtomicT,
                                                 SharedExclusiveT, ThreadId)
import           Pos.Reporting.MemState         (ReportingContext)
import           System.Wlog                    (CanLog, HasLoggerName, LoggerNameBox,
                                                 WithLogger)

import           Pos.Block.BListener            (BListenerStub, MonadBListener)
import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Client.Txp.Balances        (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History         (MonadTxHistory (..), deriveAddrHistory)
import           Pos.Communication              (TxMode)
import           Pos.Communication.PeerState    (PeerStateCtx, PeerStateRedirect,
                                                 PeerStateTag, WithPeerState)
import           Pos.Constants                  (blkSecurityParam)
import qualified Pos.Context                    as PC
import           Pos.Core                       (ChainDifficulty, HeaderHash, difficultyL,
                                                 flattenEpochOrSlot, flattenSlotId)
import           Pos.DB                         (DBPureRedirect, MonadBlockDBGeneric (..),
                                                 MonadDBRead, MonadGState, MonadRealDB,
                                                 NodeDBs)
import           Pos.DB.Block                   (BlockDBRedirect, MonadBlockDB)
import           Pos.DB.DB                      (getTipHeader)
import           Pos.Discovery                  (DiscoveryConstT, DiscoveryKademliaT,
                                                 MonadDiscovery)
import           Pos.Shutdown                   (MonadShutdownMem, triggerShutdown)
import           Pos.Slotting                   (MonadSlots (..),
                                                 getLastKnownSlotDuration)
import           Pos.Ssc.Class                  (Ssc)
import           Pos.Txp                        (filterUtxoByAddrs, runUtxoStateT)
import           Pos.Update                     (ConfirmedProposalState (..))
import           Pos.Update.Context             (UpdateContext (ucUpdateSemaphore))
import           Pos.Util.TimeWarp              (CanJsonLog, JsonLogT)
import           Pos.Util.Util                  (PowerLift (..))
import           Pos.Wallet.KeyStorage          (KeyData, MonadKeys)
import qualified Pos.Wallet.State               as WS
import           Pos.Wallet.State.Acidic        (WalletState)
import           Pos.Wallet.State.Core          (GStateCoreWalletRedirect)

data BalancesWalletRedirectTag

type BalancesWalletRedirect =
    Ether.TaggedTrans BalancesWalletRedirectTag IdentityT

runBalancesWalletRedirect :: BalancesWalletRedirect m a -> m a
runBalancesWalletRedirect = coerce

instance
    (MonadIO m, t ~ IdentityT, Ether.MonadReader' WS.WalletState m) =>
        MonadBalances (Ether.TaggedTrans BalancesWalletRedirectTag t m)
  where
    getOwnUtxos addrs = filterUtxoByAddrs addrs <$> WS.getUtxo
    getBalance = getBalanceFromUtxo

-- | Get tx history for Address
data TxHistoryWalletRedirectTag

type TxHistoryWalletRedirect =
    Ether.TaggedTrans TxHistoryWalletRedirectTag IdentityT

runTxHistoryWalletRedirect :: TxHistoryWalletRedirect m a -> m a
runTxHistoryWalletRedirect = coerce

instance
    ( MonadIO m
    , t ~ IdentityT
    , Ether.MonadReader' WS.WalletState m
    ) => MonadTxHistory (Ether.TaggedTrans TxHistoryWalletRedirectTag t m)
  where
    getTxHistory = Tagged $ \addrs _ -> do
        chain <- WS.getBestChain
        utxo <- WS.getOldestUtxo
        _ <- fmap (fst . fromMaybe (error "deriveAddrHistory: Nothing")) $
            runMaybeT $ flip runUtxoStateT utxo $
            deriveAddrHistory addrs chain
        pure $ error "getTxHistory is not implemented for light wallet"
    saveTx _ = pure ()

class Monad m => MonadBlockchainInfo m where
    networkChainDifficulty :: m (Maybe ChainDifficulty)
    localChainDifficulty :: m ChainDifficulty
    blockchainSlotDuration :: m Millisecond
    connectedPeers :: m Word

    default networkChainDifficulty
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m (Maybe ChainDifficulty)
    networkChainDifficulty = lift networkChainDifficulty

    default localChainDifficulty
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m ChainDifficulty
    localChainDifficulty = lift localChainDifficulty

    default blockchainSlotDuration
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m Millisecond
    blockchainSlotDuration = lift blockchainSlotDuration

    default connectedPeers
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m Word
    connectedPeers = lift connectedPeers

instance {-# OVERLAPPABLE #-}
    (MonadBlockchainInfo m, MonadTrans t, Monad (t m)) =>
        MonadBlockchainInfo (t m)

downloadHeader
    :: (Ssc ssc, MonadIO m, PC.MonadProgressHeader ssc m)
    => m (Maybe (BlockHeader ssc))
downloadHeader = do
    atomically . tryReadTMVar =<< Ether.ask @PC.ProgressHeaderTag

-- | Stub instance for lite-wallet
data BlockchainInfoNotImplementedTag

type BlockchainInfoNotImplemented =
    Ether.TaggedTrans BlockchainInfoNotImplementedTag IdentityT

runBlockchainInfoNotImplemented :: BlockchainInfoNotImplemented m a -> m a
runBlockchainInfoNotImplemented = coerce

instance
    (t ~ IdentityT, Monad m) =>
        MonadBlockchainInfo (Ether.TaggedTrans BlockchainInfoNotImplementedTag t m)
  where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers = error "notImplemented"


data BlockchainInfoRedirectTag

type BlockchainInfoRedirect =
    Ether.TaggedTrans BlockchainInfoRedirectTag IdentityT

runBlockchainInfoRedirect :: BlockchainInfoRedirect m a -> m a
runBlockchainInfoRedirect = coerce

getLastKnownHeader
  :: (PC.MonadLastKnownHeader ssc m, MonadIO m)
  => m (Maybe (BlockHeader ssc))
getLastKnownHeader =
    atomically . readTVar =<< Ether.ask @PC.LastKnownHeaderTag

-- | Instance for full-node's ContextHolder
instance
    ( MonadBlockDB ssc m
    , t ~ IdentityT
    , PC.MonadLastKnownHeader ssc m
    , PC.MonadProgressHeader ssc m
    , Ether.MonadReader' PC.ConnectedPeers m
    , MonadIO m
    , MonadRealDB m
    , MonadSlots m
    ) => MonadBlockchainInfo (Ether.TaggedTrans BlockchainInfoRedirectTag t m)
  where
    networkChainDifficulty = getLastKnownHeader >>= \case
        Just lh -> do
            thDiff <- view difficultyL <$> getTipHeader @(Block ssc)
            let lhDiff = lh ^. difficultyL
            return . Just $ max thDiff lhDiff
        Nothing -> runMaybeT $ do
            cSlot <- flattenSlotId <$> MaybeT getCurrentSlot
            th <- lift (getTipHeader @(Block ssc))
            let hSlot = flattenEpochOrSlot th
            when (hSlot <= cSlot - blkSecurityParam) $
                fail "Local tip is outdated"
            return $ th ^. difficultyL

    localChainDifficulty = downloadHeader >>= \case
        Just dh -> return $ dh ^. difficultyL
        Nothing -> view difficultyL <$> getTipHeader @(Block ssc)

    connectedPeers = fromIntegral . length <$> do
        PC.ConnectedPeers cp <- Ether.ask'
        atomically (readTVar cp)

    blockchainSlotDuration = getLastKnownSlotDuration

-- | Abstraction over getting update proposals
class Monad m => MonadUpdates m where
    waitForUpdate :: m ConfirmedProposalState
    applyLastUpdate :: m ()

    default waitForUpdate :: (MonadTrans t, MonadUpdates m', t m' ~ m)
                          => m ConfirmedProposalState
    waitForUpdate = lift waitForUpdate

    default applyLastUpdate :: (MonadTrans t, MonadUpdates m', t m' ~ m)
                            => m ()
    applyLastUpdate = lift applyLastUpdate

instance {-# OVERLAPPABLE #-}
    (MonadUpdates m, MonadTrans t, Monad (t m)) =>
        MonadUpdates (t m)

-- | Dummy instance for lite-wallet
data UpdatesNotImplementedTag

type UpdatesNotImplemented =
    Ether.TaggedTrans UpdatesNotImplementedTag IdentityT

runUpdatesNotImplemented :: UpdatesNotImplemented m a -> m a
runUpdatesNotImplemented = coerce

instance
    ( t ~ IdentityT
    , MonadIO m
    ) => MonadUpdates (Ether.TaggedTrans UpdatesNotImplementedTag t m)
  where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

data UpdatesRedirectTag

type UpdatesRedirect = Ether.TaggedTrans UpdatesRedirectTag IdentityT

runUpdatesRedirect :: UpdatesRedirect m a -> m a
runUpdatesRedirect = coerce

-- | Instance for full node
instance
    ( MonadIO m
    , WithLogger m
    , t ~ IdentityT
    , MonadShutdownMem m
    , Ether.MonadReader' UpdateContext m
    ) => MonadUpdates (Ether.TaggedTrans UpdatesRedirectTag t m)
  where
    waitForUpdate = takeMVar =<< Ether.asks' ucUpdateSemaphore
    applyLastUpdate = triggerShutdown

---------------------------------------------------------------
-- Composite restrictions
---------------------------------------------------------------

type WalletMode m
    = ( TxMode m
      , MonadKeys m
      , MonadBlockchainInfo m
      , MonadUpdates m
      , WithPeerState m
      , MonadDiscovery m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------

type RawWalletMode' =
    BListenerStub (
    BlockchainInfoNotImplemented (
    UpdatesNotImplemented (
    PeerStateRedirect (
    GStateCoreWalletRedirect (
    BalancesWalletRedirect (
    TxHistoryWalletRedirect (
    BlockDBRedirect (
    DBPureRedirect (
    Ether.ReadersT
        ( Tagged PeerStateTag (PeerStateCtx Production)
        , Tagged KeyData KeyData
        , Tagged WalletState WalletState
        , Tagged ReportingContext ReportingContext
        ) (
    LoggerNameBox (
    JsonLogT (
    Production
    ))))))))))))

newtype RawWalletMode a = RawWalletMode (RawWalletMode' a)
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
type instance ThreadId (RawWalletMode) = ThreadId Production
type instance Promise (RawWalletMode) = Promise Production
type instance SharedAtomicT (RawWalletMode) = SharedAtomicT Production
type instance SharedExclusiveT (RawWalletMode) = SharedExclusiveT Production
type instance Gauge (RawWalletMode) = Gauge Production
type instance ChannelT (RawWalletMode) = ChannelT Production
type instance Distribution (RawWalletMode) = Distribution Production
type instance Counter (RawWalletMode) = Counter Production

deriving instance CanLog (RawWalletMode)
deriving instance HasLoggerName (RawWalletMode)
--deriving instance MonadSlotsData (RawWalletMode)
--deriving instance MonadSlots (RawWalletMode)
deriving instance MonadGState (RawWalletMode)
deriving instance Ether.MonadReader' NodeDBs Production => MonadDBRead (RawWalletMode)
deriving instance MonadBListener (RawWalletMode)
deriving instance MonadUpdates (RawWalletMode)
deriving instance MonadBlockchainInfo (RawWalletMode)
deriving instance MonadBalances (RawWalletMode)
deriving instance MonadTxHistory (RawWalletMode)
deriving instance WithPeerState (RawWalletMode)
deriving instance CanJsonLog (RawWalletMode)

instance PowerLift m (RawWalletMode') => PowerLift m (RawWalletMode) where
  powerLift = RawWalletMode . powerLift

instance
    ( Ether.MonadReader' NodeDBs Production
    , MonadBlockDBGeneric header blk undo (RawWalletMode') ) =>
    MonadBlockDBGeneric header blk undo (RawWalletMode) where
    dbGetHeader = (coerce :: (HeaderHash -> RawWalletMode' (Maybe header)) ->
                             (HeaderHash -> RawWalletMode (Maybe header)))
                  (dbGetHeader @header @blk @undo)
    dbGetBlock = (coerce :: (HeaderHash -> RawWalletMode' (Maybe blk)) ->
                            (HeaderHash -> RawWalletMode (Maybe blk)))
                 (dbGetBlock @header @blk @undo)
    dbGetUndo = (coerce :: (HeaderHash -> RawWalletMode' (Maybe undo)) ->
                           (HeaderHash -> RawWalletMode (Maybe undo)))
                 (dbGetUndo @header @blk @undo)

instance
    ( Mockable d (RawWalletMode')
    , MFunctor' d (RawWalletMode) (RawWalletMode')
    )
    => Mockable d (RawWalletMode) where
    liftMockable dmt = RawWalletMode $ liftMockable $ hoist' (\(RawWalletMode m) -> m) dmt

instance
    Ether.MonadReader tag r (RawWalletMode') =>
    Ether.MonadReader tag r (RawWalletMode)
  where
    ask =
        (coerce :: RawWalletMode' r -> RawWalletMode r)
        (Ether.ask @tag)
    local =
        (coerce :: forall a .
            Lift.Local r (RawWalletMode') a ->
            Lift.Local r (RawWalletMode) a)
        (Ether.local @tag)
    reader =
        (coerce :: forall a .
            ((r -> a) -> RawWalletMode' a) ->
            ((r -> a) -> RawWalletMode a))
        (Ether.reader @tag)

type WalletRealMode = DiscoveryKademliaT RawWalletMode

type WalletStaticPeersMode = DiscoveryConstT RawWalletMode
