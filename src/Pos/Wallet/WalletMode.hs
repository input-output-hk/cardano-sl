{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , WalletMode

       -- * Monadic redirect
       , BlockchainInfoRedirect
       , runBlockchainInfoRedirect
       , UpdatesRedirect
       , runUpdatesRedirect
       ) where

import           Universum

import           Control.Concurrent.STM       (tryReadTMVar)
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Coerce                  (coerce)
import           Data.Time.Units              (Millisecond)
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, BlockHeader)
import           Pos.Client.Txp.Balances      (MonadBalances (..))
import           Pos.Client.Txp.History       (MonadTxHistory (..))
import           Pos.Communication            (TxMode)
import           Pos.Communication.PeerState  (WithPeerState)
import           Pos.Constants                (blkSecurityParam)
import qualified Pos.Context                  as PC
import           Pos.Core                     (ChainDifficulty, difficultyL,
                                               flattenEpochOrSlot, flattenSlotId)
import           Pos.DB                       (MonadRealDB)
import           Pos.DB.Block                 (MonadBlockDB)
import           Pos.DB.DB                    (getTipHeader)
import           Pos.Discovery                (MonadDiscovery)
import           Pos.Shutdown                 (MonadShutdownMem, triggerShutdown)
import           Pos.Slotting                 (MonadSlots (..), getLastKnownSlotDuration)
import           Pos.Ssc.Class                (Ssc)
import           Pos.Update                   (ConfirmedProposalState (..))
import           Pos.Update.Context           (UpdateContext (ucUpdateSemaphore))
import           Pos.Wallet.KeyStorage        (MonadKeys)

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
      , MonadResource m
      )
