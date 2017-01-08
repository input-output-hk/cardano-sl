-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , makeMOfNTx
       , submitTx
       , submitTxRaw
       , createTx
       , createMOfNTx
       ) where

import           Control.Lens          ((^.), _1)
import           Control.Monad.Except  (ExceptT (..), runExceptT)
import           Control.TimeWarp.Rpc  (NetworkAddress)
import           Formatting            (build, sformat, (%))
import           System.Wlog           (logError, logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication     (sendTx)
import           Pos.Crypto            (SecretKey, hash, toPublic)
import           Pos.Types             (TxAux, TxOutAux, makePubKeyAddress, txaF)
import           Pos.WorkMode          (NewMinWorkMode)

import           Pos.Wallet.Tx.Pure    (TxError, createMOfNTx, createTx, makeMOfNTx,
                                        makePubKeyTx)
import           Pos.Wallet.WalletMode (TxMode, getOwnUtxo, saveTx)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => SecretKey
    -> [NetworkAddress]
    -> [TxOutAux]
    -> m (Either TxError TxAux)
submitTx _ [] _ = logError "No addresses to send" >> fail "submitTx failed"
submitTx sk na outputs = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ toPublic sk
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo sk outputs
        let txId = hash (txw ^. _1)
        lift $ submitTxRaw na txw
        lift $ saveTx (txId, txw)
        return txw

-- | Send the ready-to-use transaction
submitTxRaw :: NewMinWorkMode m => [NetworkAddress] -> TxAux -> m ()
submitTxRaw na tx = do
    let txId = hash (tx ^. _1)
    logInfo $ sformat ("Submitting transaction: "%txaF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    -- [CSL-447] TODO Uncomment
    --mapM_ (`sendTx` tx) na
