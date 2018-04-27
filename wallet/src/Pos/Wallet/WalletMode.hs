{-# LANGUAGE TypeFamilies #-}

-- | 'MonadWallet' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , MonadWallet
       , WalletMempoolExt
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)
import           Data.Time.Units (Millisecond)

import           Pos.Client.KeyStorage (MonadKeys)
import           Pos.Client.Txp.History (MonadTxHistory (..))
import           Pos.Client.Txp.Network (TxMode)
import           Pos.Core (ChainDifficulty)
import           Pos.Update (ConfirmedProposalState (..))
import           Pos.Util.TimeWarp (CanJsonLog)
import           Pos.WorkMode (EmptyMempoolExt)

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

type WalletMempoolExt = EmptyMempoolExt

---------------------------------------------------------------
-- Composite restrictions
---------------------------------------------------------------

type MonadWallet m
    = ( TxMode m
      , MonadKeys m
      , MonadBlockchainInfo m
      , MonadUpdates m
      , CanJsonLog m
      )
