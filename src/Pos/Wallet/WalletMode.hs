{-# LANGUAGE TypeFamilies #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , WalletMode
       ) where

import           Universum

import           Control.Monad.Trans         (MonadTrans)
import           Data.Time.Units             (Millisecond)

import           Pos.Client.Txp.Balances     (MonadBalances (..))
import           Pos.Client.Txp.History      (MonadTxHistory (..))
import           Pos.Communication           (TxMode)
import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Core                    (ChainDifficulty)
import           Pos.Discovery               (MonadDiscovery)
import           Pos.Update                  (ConfirmedProposalState (..))
import           Pos.Wallet.KeyStorage       (MonadKeys)

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
