-- | Functions for operating with transactions

module Pos.Communication.Tx
       ( TxMode
       , submitTx
       , submitMTx
       , submitRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Control.Monad.Except       (runExceptT)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable, mapConcurrently)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Client.Txp.Balances    (MonadBalances (..), getOwnUtxo)
import           Pos.Client.Txp.History     (MonadTxHistory (..))
import           Pos.Client.Txp.Util        (TxCreateMode, TxError (..), createMTx,
                                             createRedemptionTx, createTx,
                                             overrideTxDistrBoot)
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (NodeId, OutSpecs, SendActions)
import           Pos.Communication.Specs    (createOutSpecs)
import           Pos.Communication.Types    (InvOrDataTK)
import           Pos.Crypto                 (RedeemSecretKey, SafeSigner, hash,
                                             redeemToPublic, safeToPublic)
import           Pos.DB.Class               (MonadGState)
import           Pos.Txp.Core               (TxAux (..), TxId, TxOut (..), TxOutAux (..),
                                             txaF)
import           Pos.Txp.Network.Types      (TxMsgContents (..))
import           Pos.Types                  (Address, Coin, makePubKeyAddress,
                                             makeRedeemAddress, mkCoin, unsafeAddCoin)
import           Pos.Util.Util              (eitherToThrow)
import           Pos.WorkMode.Class         (MinWorkMode)

type TxMode ssc ctx m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory ssc m
      , MonadMockable m
      , MonadMask m
      , MonadThrow m
      , TxCreateMode ctx m
      )

submitAndSave
    :: TxMode ssc ctx m
    => SendActions m -> [NodeId] -> TxAux -> m TxAux
submitAndSave sendActions na txAux@TxAux {..} = do
    let txId = hash taTx
    submitTxRaw sendActions na txAux
    saveTx (txId, txAux)
    return txAux

-- | Construct Tx using multiple secret keys and given list of desired outputs.
submitMTx
    :: TxMode ssc ctx m
    => SendActions m
    -> NonEmpty (SafeSigner, Address)
    -> [NodeId]
    -> NonEmpty TxOutAux
    -> m TxAux
submitMTx sendActions hdwSigner na outputs = do
    let addrs = map snd $ toList hdwSigner
    utxo <- getOwnUtxos addrs
    txw <- eitherToThrow TxError $
           createMTx utxo hdwSigner outputs
    submitAndSave sendActions na txw

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc ctx m
    => SendActions m
    -> SafeSigner
    -> [NodeId]
    -> NonEmpty TxOutAux
    -> m TxAux
submitTx sendActions ss na outputs = do
    utxo <- getOwnUtxos . one $ makePubKeyAddress (safeToPublic ss)
    createTx utxo ss outputs >>= \case
        Left e -> throwM (TxError e)
        Right txw -> submitAndSave sendActions na txw

-- | Construct redemption Tx using redemption secret key and a output address
submitRedemptionTx
    :: TxMode ssc ctx m
    => SendActions m
    -> RedeemSecretKey
    -> [NodeId]
    -> Address
    -> m (TxAux, Address, Coin)
submitRedemptionTx sendActions rsk na output = do
    let redeemAddress = makeRedeemAddress $ redeemToPublic rsk
    utxo <- getOwnUtxo redeemAddress
    let addCoin c = unsafeAddCoin c . txOutValue . toaOut
        redeemBalance = foldl' addCoin (mkCoin 0) utxo
        txOuts0 = one $
            TxOutAux {toaOut = TxOut output redeemBalance, toaDistr = []}
    when (redeemBalance == mkCoin 0) $ throwM . TxError $ "Redeem balance is 0"
    txOuts <- eitherToThrow TxError =<< runExceptT (overrideTxDistrBoot txOuts0)
    txw <- eitherToThrow TxError $ createRedemptionTx utxo rsk txOuts
    txAux <- submitAndSave sendActions na txw
    pure (txAux, redeemAddress, redeemBalance)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadGState m, MonadThrow m)
    => SendActions m -> [NodeId] -> TxAux -> m ()
submitTxRaw sa na txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    void $ mapConcurrently (flip (sendTx sa) txAux) na

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))
