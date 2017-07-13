-- | Functions for operating with transactions

module Pos.Communication.Tx
       ( TxMode
       , submitTx
       , submitMTx
       , submitRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Control.Monad.Except       (ExceptT (..), runExceptT, throwError)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable, mapConcurrently)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Client.Txp.Balances    (MonadBalances (..), getOwnUtxo)
import           Pos.Client.Txp.History     (MonadTxHistory (..))
import           Pos.Client.Txp.Util        (TxError, createMTx, createRedemptionTx,
                                             createTx)
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
import           Pos.WorkMode.Class         (MinWorkMode)

type TxMode m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory m
      , MonadMockable m
      , MonadMask m
      , MonadGState m
      )

submitAndSave
    :: TxMode m
    => SendActions m -> [NodeId] -> TxAux -> ExceptT TxError m TxAux
submitAndSave sendActions na txAux@TxAux {..} = do
    let txId = hash taTx
    lift $ submitTxRaw sendActions na txAux
    lift $ saveTx (txId, txAux)
    return txAux

-- | Construct Tx using multiple secret keys and given list of desired outputs.
submitMTx
    :: TxMode m
    => SendActions m
    -> NonEmpty (SafeSigner, Address)
    -> [NodeId]
    -> NonEmpty TxOutAux
    -> m (Either TxError TxAux)
submitMTx sendActions hdwSigner na outputs = do
    let addrs = map snd $ toList hdwSigner
    utxo <- getOwnUtxos addrs
    runExceptT $ do
        txw <- ExceptT $ return $ createMTx utxo hdwSigner outputs
        submitAndSave sendActions na txw

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode m
    => SendActions m
    -> SafeSigner
    -> [NodeId]
    -> NonEmpty TxOutAux
    -> m (Either TxError TxAux)
submitTx sendActions ss na outputs = do
    utxo <- getOwnUtxos . one $ makePubKeyAddress (safeToPublic ss)
    runExceptT $ do
        txw <- ExceptT $ return $ createTx utxo ss outputs
        submitAndSave sendActions na txw

-- | Construct redemption Tx using redemption secret key and a output address
submitRedemptionTx
    :: TxMode m
    => SendActions m
    -> RedeemSecretKey
    -> [NodeId]
    -> Address
    -> m (Either TxError (TxAux, Address, Coin))
submitRedemptionTx sendActions rsk na output = do
    let redeemAddress = makeRedeemAddress $ redeemToPublic rsk
    utxo <- getOwnUtxo redeemAddress
    runExceptT $ do
        let addCoin c = unsafeAddCoin c . txOutValue . toaOut
            redeemBalance = foldl' addCoin (mkCoin 0) utxo
            txouts =
                one $
                TxOutAux {toaOut = TxOut output redeemBalance, toaDistr = []}
        when (redeemBalance == mkCoin 0) $
            throwError "Redeem balance is 0"
        txw <- ExceptT $ return $ createRedemptionTx utxo rsk txouts
        txAux <- submitAndSave sendActions na txw
        pure (txAux, redeemAddress, redeemBalance)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadGState m)
    => SendActions m -> [NodeId] -> TxAux -> m ()
submitTxRaw sa na txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    void $ mapConcurrently (flip (sendTx sa) txAux) na

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))
