{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       ( announceBlock
       , announceTx
       , announceTxs
       , sendToNeighborsSafe
       , sendTx
       ) where

import           Control.TimeWarp.Rpc    (Message, NetworkAddress)
import           Control.TimeWarp.Timed  (fork_)
import           Data.Binary             (Binary)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import           Formatting              (bprint, build, sformat, (%))
import           System.Wlog             (logDebug)
import           Universum

import           Pos.Communication.Types (SendBlockHeader (..), SendTx (..), SendTxs (..))
import           Pos.DHT                 (sendToNeighbors, sendToNode)
import           Pos.Types               (MainBlockHeader, Tx, TxWitness, txwF)
import           Pos.Util                (logWarningWaitLinear, messageName')
import           Pos.WorkMode            (WorkMode)
import           Serokell.Util.Text      (listJson)

-- | Wrapper on top of sendToNeighbors which does it in separate
-- thread and controls how much time action takes.
sendToNeighborsSafe :: (Binary r, Message r, WorkMode ssc m) => r -> m ()
sendToNeighborsSafe msg = do
    let msgName = messageName' msg
    let action = () <$ sendToNeighbors msg
    fork_ $
        logWarningWaitLinear 10 ("Sending " <> msgName <> " to neighbors") action

-- | Announce new block to all known peers. Intended to be used when
-- block is created.
announceBlock
    :: WorkMode ssc m
    => MainBlockHeader ssc -> m ()
announceBlock header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    sendToNeighborsSafe . SendBlockHeader $ header

-- | Announce new transaction to all known peers. Intended to be used when
-- tx is created.
announceTx :: WorkMode ssc m => (Tx, TxWitness) -> m ()
announceTx txw@(tx,w) = do
    logDebug $ sformat ("Announcing tx to others:\n"%txwF) txw
    sendToNeighborsSafe $ SendTx tx w

-- | Announce known transactions to all known peers. Intended to be used
-- to relay transactions.
announceTxs :: WorkMode ssc m => [(Tx,TxWitness)] -> m ()
announceTxs [] = pure ()
announceTxs txs@(tx:txs') = do
    logDebug $
        sformat ("Announcing txs to others:\n" %listJson)
                (fmap (bprint txwF) txs)
    sendToNeighborsSafe . SendTxs $ tx :| txs'

-- | Send Tx to given address.
sendTx :: WorkMode ssc m => NetworkAddress -> (Tx, TxWitness) -> m ()
sendTx addr (tx,w) = sendToNode addr $ SendTx tx w
