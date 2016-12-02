{-# LANGUAGE FlexibleContexts #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , submitTx
       , submitTxRaw
       ) where

import           Control.TimeWarp.Rpc   (NetworkAddress)
import           Control.TimeWarp.Timed (currentTime, for, fork, sleepForever, wait)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (logError, logInfo)
import           Universum

import           Pos.Communication      (sendTx)
import           Pos.Crypto             (hash)
import           Pos.DHT                (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.State              (initFirstSlot)
import           Pos.Types              (Address, Coin, Timestamp (Timestamp), Tx (..),
                                         TxId, txF)
import           Pos.Util               (inAssertMode)
import           Pos.Wallet             (makePubKeyTx)
import           Pos.Worker             (runWorkers)
import           Pos.WorkMode           (NodeContext (..), WorkMode, getNodeContext,
                                         ncPublicKey)

-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, WorkMode ssc m) => [m ()] -> m ()
runNode plugins = do
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    initFirstSlot
    waitSystemStart
    runWorkers
    mapM_ fork plugins
    sleepForever

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m Tx
submitTx na input output =
    if null na
      then logError "No addresses to send" >> panic "submitTx failed"
      else do
        pk <- ncPublicKey <$> getNodeContext
        sk <- ncSecretKey <$> getNodeContext
        let tx = makePubKeyTx pk sk [input] [output]
        submitTxRaw na tx
        pure tx

-- | Send the ready-to-use transaction
submitTxRaw :: WorkMode ssc m => [NetworkAddress] -> Tx -> m ()
submitTxRaw na tx = do
    let txId = hash tx
    logInfo $ sformat ("Submitting transaction: "%txF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ wait (for (start - cur))
