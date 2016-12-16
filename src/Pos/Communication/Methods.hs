{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       (
       -- * Sending data into network
         announceBlock
       , sendToNeighborsSafe
       , sendTx
       , sendProxySecretKey

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
import           Pos.Binary.Communication    ()
import           Pos.Binary.Txp              ()
import           Pos.Binary.Types            ()
import           Pos.Communication.Types     (RequestBlockchainPart (..),
                                              SendBlockHeader (..),
                                              SendProxySecretKey (..))
import           Pos.Crypto                  (ProxySecretKey)
import           Pos.DHT.Model               (MonadMessageDHT, sendToNeighbors,
                                              sendToNode)
import           Pos.Txp.Types.Communication (TxDataMsg (..))
import           Pos.Types                   (EpochIndex, HeaderHash, MainBlockHeader, Tx,
                                              TxWitness, headerHash)
import           Pos.Util                    (logWarningWaitLinear, messageName')
import           Pos.WorkMode                (WorkMode)

-- | Wrapper on top of sendToNeighbors which does it in separate
-- thread and controls how much time action takes.
sendToNeighborsSafe :: (Bi r, Message r, WorkMode ssc m) => r -> m ()
sendToNeighborsSafe msg = do
    let msgName = messageName' msg
    let action = () <$ sendToNeighbors msg
    fork_ $
        logWarningWaitLinear 10 ("Sending " <> msgName <> " to neighbors") action

-- | Announce new block to all known peers. Intended to be used when
-- block is created.
announceBlock :: (WorkMode ssc m) => MainBlockHeader ssc -> m ()
announceBlock header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    sendToNeighborsSafe . SendBlockHeader $ header

-- | Query the blockchain part. Generic method.
queryBlockchainPart
    :: (WorkMode ssc m)
    => Maybe (HeaderHash ssc) -> Maybe (HeaderHash ssc) -> Maybe Word
    -> m ()
queryBlockchainPart fromH toH mLen = do
    logDebug $ sformat ("Querying blockchain part "%build%".."%build%
                        " (maxlen "%build%")") fromH toH mLen
    sendToNeighborsSafe $ RequestBlockchainPart fromH toH mLen

-- | Query for all the newest blocks until some given hash
queryBlockchainUntil :: (WorkMode ssc m) => HeaderHash ssc -> m ()
queryBlockchainUntil hash = queryBlockchainPart Nothing (Just hash) Nothing

-- | Query for possible new blocks on top of new blockchain.
queryBlockchainFresh :: (WorkMode ssc m) => m ()
queryBlockchainFresh = queryBlockchainUntil . headerHash =<< getHeadBlock

-- | Send Tx to given address.
sendTx :: (MonadMessageDHT s m) => NetworkAddress -> (Tx, TxWitness) -> m ()
sendTx addr (tx,w) = sendToNode addr $ TxDataMsg tx w

-- | Sends proxy secret key to neighbours
sendProxySecretKey
    :: (WorkMode ssc m)
    => ProxySecretKey (EpochIndex, EpochIndex) -> m ()
sendProxySecretKey psk = do
    logDebug $ sformat ("Sending proxySecretKey to neigbours:\n"%build) psk
    sendToNeighborsSafe $ SendProxySecretKey psk
