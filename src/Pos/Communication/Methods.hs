{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       ( announceBlock
       , sendToNeighborsSafe
       , sendToNeighborsSafe
       , sendToNeighborsSafeWithMaliciousEmulation
       , sendTx

       -- * Blockchain part queries
       , queryBlockchainPart
       , queryBlockchainUntil
       , queryBlockchainFresh
       ) where

import           Control.TimeWarp.Rpc        (Message, NetworkAddress)
import           Control.TimeWarp.Timed      (fork_)
import           Formatting                  (build, sformat, (%))
import           Pos.State                   (getHeadBlock)
import           System.Wlog                 (logDebug)
import           Universum

import           Pos.Binary.Class            (Bi)
import           Pos.Communication.Types     (RequestBlockchainPart (..),
                                              SendBlockHeader (..))
import           Pos.Context                 (getNodeContext, ncMalicious)
import           Pos.DHT                     (MonadMessageDHT,
                                              sendToNode, defaultSendToNeighbors)
import           Pos.Txp.Types.Communication (TxDataMsg (..))
import           Pos.Types                   (HeaderHash, MainBlockHeader, Tx, TxWitness,
                                              headerHash)
import           Pos.Util                    (logWarningWaitLinear, messageName')
import           Pos.WorkMode                (WorkMode)

-- | Wrapper on top of sendToNeighbors which does it in separate
-- thread and controls how much time action takes.
sendToNeighborsSafeImpl :: (Bi r, Message r, WorkMode ssc m) => Bool -> r -> m ()
sendToNeighborsSafeImpl emulateMaliciousActions msg = do
    let msgName = messageName' msg
    let sendToNode' = if emulateMaliciousActions then sendMalicious else sendToNode
    -- TODO(voit): sendToNeighbors should be done using seqConcurrentlyK
    let action = () <$ defaultSendToNeighbors sequence sendToNode' msg
    fork_ $
        logWarningWaitLinear 10 ("Sending " <> msgName <> " to neighbors") action
  where
    sendMalicious addr message = do
        malicious <- ncMalicious <$> getNodeContext
        when (addr `notElem` malicious) $
            sendToNode addr message

sendToNeighborsSafe :: (Bi r, Message r, WorkMode ssc m) => r -> m ()
sendToNeighborsSafe = sendToNeighborsSafeImpl False

sendToNeighborsSafeWithMaliciousEmulation :: (Bi r, Message r, WorkMode ssc m) => r -> m ()
sendToNeighborsSafeWithMaliciousEmulation = sendToNeighborsSafeImpl True

-- | Announce new block to all known peers. Intended to be used when
-- block is created.
announceBlock
    :: (WorkMode ssc m, Bi (SendBlockHeader ssc))
    => MainBlockHeader ssc -> m ()
announceBlock header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    sendToNeighborsSafeWithMaliciousEmulation . SendBlockHeader $ header

-- | Query the blockchain part. Generic method.
queryBlockchainPart
    :: (WorkMode ssc m, Bi (RequestBlockchainPart ssc))
    => Maybe (HeaderHash ssc) -> Maybe (HeaderHash ssc) -> Maybe Word
    -> m ()
queryBlockchainPart fromH toH mLen = do
    logDebug $ sformat ("Querying blockchain part "%build%".."%build%
                        " (maxlen "%build%")") fromH toH mLen
    sendToNeighborsSafe $ RequestBlockchainPart fromH toH mLen

-- | Query for all the newest blocks until some given hash
queryBlockchainUntil
    :: (WorkMode ssc m, Bi (RequestBlockchainPart ssc))
    => HeaderHash ssc -> m ()
queryBlockchainUntil hash = queryBlockchainPart Nothing (Just hash) Nothing

-- | Query for possible new blocks on top of new blockchain.
queryBlockchainFresh
    :: (WorkMode ssc m, Bi (RequestBlockchainPart ssc))
    => m ()
queryBlockchainFresh = queryBlockchainUntil . headerHash =<< getHeadBlock

-- | Send Tx to given address.
sendTx
    :: (MonadMessageDHT s m, Bi TxDataMsg)
    => NetworkAddress -> (Tx, TxWitness) -> m ()
sendTx addr (tx,w) = sendToNode addr $ TxDataMsg tx w
