{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       ( announceBlock
       , sendToNeighborsSafe
       , sendTx
       ) where

import           Control.TimeWarp.Rpc        (Message, NetworkAddress)
import           Control.TimeWarp.Timed      (fork_)
import           Data.Binary                 (Binary)
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (logDebug)
import           Universum

import           Pos.Communication.Types     (SendBlockHeader (..))
import           Pos.DHT                     (sendToNeighbors, sendToNode)
import           Pos.Txp.Types.Communication (TxDataMsg (..))
import           Pos.Types                   (MainBlockHeader, Tx, TxWitness)
import           Pos.Util                    (logWarningWaitLinear, messageName')
import           Pos.WorkMode                (WorkMode)

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

-- | Send Tx to given address.
sendTx :: WorkMode ssc m => NetworkAddress -> (Tx, TxWitness) -> m ()
sendTx addr (tx,w) = sendToNode addr $ TxDataMsg tx w
