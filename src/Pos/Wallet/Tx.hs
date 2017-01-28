-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , makeMOfNTx
       , submitTx
       , submitTxRaw
       , createTx
       , createMOfNTx
       ) where

import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (mapConcurrently)
import           System.Wlog                (logError, logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (SendActions)
import           Pos.Crypto                 (SecretKey, hash, toPublic)
import           Pos.DHT.Model              (DHTNode)
import           Pos.Types                  (TxAux, TxOutAux, makePubKeyAddress, txaF)
import           Pos.Wallet.Tx.Pure         (TxError, createMOfNTx, createTx, makeMOfNTx,
                                             makePubKeyTx)
import           Pos.Wallet.WalletMode      (TxMode, getOwnUtxo, saveTx)
import           Pos.WorkMode               (MinWorkMode)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => SendActions m
    -> SecretKey
    -> [DHTNode]
    -> [TxOutAux]
    -> m (Either TxError TxAux)
submitTx _ _ [] _ = do
    logError "No addresses to send"
    return (Left "submitTx failed")
submitTx sendActions sk na outputs = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ toPublic sk
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo sk outputs
        let txId = hash (txw ^. _1)
        lift $ submitTxRaw sendActions na txw
        lift $ saveTx (txId, txw)
        return txw

-- | Send the ready-to-use transaction
submitTxRaw :: MinWorkMode m => SendActions m -> [DHTNode] -> TxAux -> m ()
submitTxRaw sa na tx = do
    let txId = hash (tx ^. _1)
    logInfo $ sformat ("Submitting transaction: "%txaF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    void $ mapConcurrently (flip (sendTx sa) tx) na
