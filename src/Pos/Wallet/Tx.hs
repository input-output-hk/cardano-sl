-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , makeMOfNTx
       , submitTx
       , submitRedemptionTx
       , submitTxRaw
       , createTx
       , createMOfNTx
       , sendTxOuts
       ) where

import           Control.Monad.Except       (ExceptT (..), runExceptT)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (mapConcurrently)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (SendActions)
import           Pos.Communication.Specs    (sendTxOuts)
import           Pos.Crypto                 (RedeemSecretKey, SafeSigner, hash,
                                             redeemToPublic, safeToPublic)
import           Pos.DB.Limits              (MonadDBLimits)
import           Pos.DHT.Model              (DHTNode)
import           Pos.Txp.Core               (TxAux, TxOut (..), TxOutAux (..), txaF)
import           Pos.Types                  (Address, makePubKeyAddress,
                                             makeRedeemAddress, mkCoin, unsafeAddCoin)
import           Pos.Wallet.Tx.Pure         (TxError, createMOfNTx, createRedemptionTx,
                                             createTx, makeMOfNTx, makePubKeyTx)
import           Pos.Wallet.WalletMode      (TxMode, getOwnUtxo, saveTx)
import           Pos.WorkMode               (MinWorkMode)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => SendActions m
    -> SafeSigner
    -> [DHTNode]
    -> NonEmpty TxOutAux
    -> m (Either TxError TxAux)
submitTx sendActions ss na outputs = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ safeToPublic ss
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo ss outputs
        let txId = hash (txw ^. _1)
        lift $ submitTxRaw sendActions na txw
        lift $ saveTx (txId, txw)
        return txw

-- | Construct redemption Tx using redemption secret key and a output address
submitRedemptionTx
    :: TxMode ssc m
    => SendActions m
    -> RedeemSecretKey
    -> [DHTNode]
    -> Address
    -> m (Either TxError TxAux)
submitRedemptionTx sendActions rsk na output = do
    utxo <- getOwnUtxo $ makeRedeemAddress $ redeemToPublic rsk
    runExceptT $ do
        let addCoin c = unsafeAddCoin c . txOutValue . toaOut
            redeemBalance = foldl' addCoin (mkCoin 0) utxo
            txouts =
                one $
                TxOutAux {toaOut = TxOut output redeemBalance, toaDistr = []}
        txw <- ExceptT $ return $ createRedemptionTx utxo rsk txouts
        let txId = hash (txw ^. _1)
        lift $ submitTxRaw sendActions na txw
        lift $ saveTx (txId, txw)
        return txw

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m -> [DHTNode] -> TxAux -> m ()
submitTxRaw sa na tx = do
    let txId = hash (tx ^. _1)
    logInfo $ sformat ("Submitting transaction: "%txaF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    void $ mapConcurrently (flip (sendTx sa) tx) na
