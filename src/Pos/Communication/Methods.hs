{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       ( announceBlock
       , announceTx
       , announceTxs
       , sendTx
       , announceSsc
       ) where

import           Control.TimeWarp.Logging (logDebug)
import           Control.TimeWarp.Rpc     (Message, NetworkAddress)
import           Control.TimeWarp.Timed   (fork_)
import           Data.Binary              (Binary)
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Formatting               (build, sformat, (%))
import           Universum

import           Pos.Communication.Types  (SendBlockHeader (..), SendTx (..),
                                           SendTxs (..))
import           Pos.DHT                  (sendToNeighbors, sendToNode)
import           Pos.Ssc.Class.Types      (SscTypes (SscMessage))
import           Pos.Types                (MainBlockHeader, Tx)
import           Pos.Util                 (logWarningWaitLinear, messageName')
import           Pos.WorkMode             (WorkMode)
import           Serokell.Util.Text       (listJson)

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
announceTx :: WorkMode ssc m => Tx -> m ()
announceTx tx = do
    logDebug $ sformat ("Announcing tx to others:\n"%build) tx
    sendToNeighborsSafe . SendTx $ tx

-- | Announce known transactions to all known peers. Intended to be used
-- to relay transactions.
announceTxs :: WorkMode ssc m => [Tx] -> m ()
announceTxs [] = pure ()
announceTxs txs@(tx:txs') = do
    logDebug $
        sformat ("Announcing txs to others:\n" %listJson) txs
    sendToNeighborsSafe . SendTxs $ tx :| txs'

-- | Send Tx to given address.
sendTx :: WorkMode ssc m => NetworkAddress -> Tx -> m ()
sendTx addr = sendToNode addr . SendTx

----------------------------------------------------------------------------
-- Relaying MPC messages
----------------------------------------------------------------------------

-- | Announce ssc message to all known peers. Intended to be used
-- by SSC algorithm
announceSsc :: (WorkMode ssc m,
                Message (SscMessage ssc)) => SscMessage ssc -> m ()
announceSsc = sendToNeighborsSafe

----------------------------------------------------------------------------
-- TODOs from the first version of the prototype, to be reviewed/deleted
----------------------------------------------------------------------------

-- * What to do about blocks delivered a bit late? E.g. let's assume that a
--   block was generated in slot X, but received by another node in slot
--   Y. What are the conditions on Y under which the block should (and
--   shouldn't) be accepted?

-- * Let's say that we receive a transaction, and then we receive a block
--   containing that transaction. We remove the transaction from our list of
--   pending transactions. Later (before K slots pass) it turns out that that
--   block was bad, and we discard it; then we should add the transaction
--   back. Right? If this is how it works, then it means that somebody can
--   prevent the transaction from being included into the blockchain for the
--   duration of K−1 slots – right? How easy/probable/important is it in
--   practice?

-- * We should exclude extremely delayed entries that are the same as ones we
--   already received before, but already included into one of the previous
--   blocks.
