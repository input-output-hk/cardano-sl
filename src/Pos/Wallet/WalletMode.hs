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

instance {-# OVERLAPPABLE #-}
    (MonadBlockchainInfo m, MonadTrans t, Monad (t m)) =>
        MonadBlockchainInfo (t m)
  where
    networkChainDifficulty = lift networkChainDifficulty
    localChainDifficulty = lift localChainDifficulty
    blockchainSlotDuration = lift blockchainSlotDuration
    connectedPeers = lift connectedPeers

-- | Abstraction over getting update proposals
class Monad m => MonadUpdates m where
    waitForUpdate :: m ConfirmedProposalState
    applyLastUpdate :: m ()

instance {-# OVERLAPPABLE #-}
    (MonadUpdates m, MonadTrans t, Monad (t m)) =>
        MonadUpdates (t m)
  where
    waitForUpdate = lift waitForUpdate
    applyLastUpdate = lift applyLastUpdate

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
