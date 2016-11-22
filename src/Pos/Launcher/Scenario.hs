{-# LANGUAGE FlexibleContexts #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , submitTx
       , submitTxRaw
       ) where

import           Control.TimeWarp.Rpc   (NetworkAddress)
import           Control.TimeWarp.Timed (currentTime, for, sleepForever, wait)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (logError, logInfo)
import           Universum

import           Pos.Communication      (sendTx)
import           Pos.Crypto             (hash, sign)
import           Pos.DHT                (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.Types              (Address, Coin, Timestamp (Timestamp), Tx (..),
                                         TxId, TxIn (..), TxOut (..), txF)
import           Pos.Worker             (runWorkers)
import           Pos.WorkMode           (NodeContext (..), WorkMode, getNodeContext,
                                         ncPublicKey)

-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, WorkMode ssc m) => m ()
runNode = do
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    waitSystemStart
    runWorkers
    sleepForever

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode ssc m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m Tx
submitTx na (txInHash, txInIndex) (txOutAddress, txOutValue) =
    if null na
      then logError "No addresses to send" >> panic "submitTx failed"
      else do
        sk <- ncSecretKey <$> getNodeContext
        let txOuts = [TxOut {..}]
            txIns = [TxIn {txInSig = sign sk (txInHash, txInIndex, txOuts), ..}]
            tx = Tx {txInputs = txIns, txOutputs = txOuts}
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
