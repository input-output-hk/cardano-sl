{-# LANGUAGE RankNTypes #-}

-- | Functions for operating with transactions

module Pos.Communication.Tx
       ( TxMode
       , submitTx
       , prepareMTx
       , prepareRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Client.Txp.Addresses   (MonadAddresses (..))
import           Pos.Client.Txp.Balances    (MonadBalances (..), getOwnUtxo,
                                             getOwnUtxoForPk)
import           Pos.Client.Txp.History     (MonadTxHistory (..))
import           Pos.Client.Txp.Util        (TxCreateMode, TxError (..), createMTx,
                                             createRedemptionTx, createTx)
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (EnqueueMsg, OutSpecs)
import           Pos.Communication.Specs    (createOutSpecs)
import           Pos.Communication.Types    (InvOrDataTK)
import           Pos.Core                   (Address, Coin, makeRedeemAddress, mkCoin,
                                             unsafeAddCoin)
import           Pos.Crypto                 (RedeemSecretKey, SafeSigner, hash,
                                             redeemToPublic, safeToPublic)
import           Pos.DB.Class               (MonadGState)
import           Pos.Txp.Core               (TxAux (..), TxId, TxOut (..), TxOutAux (..),
                                             txaF)
import           Pos.Txp.Network.Types      (TxMsgContents (..))
import           Pos.Util.Util              (eitherToThrow)
import           Pos.WorkMode.Class         (MinWorkMode)

type TxMode ssc m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory ssc m
      , MonadMockable m
      , MonadMask m
      , MonadThrow m
      , TxCreateMode m
      )

submitAndSave
    :: TxMode ssc m
    => EnqueueMsg m -> TxAux -> m Bool
submitAndSave enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    accepted <- submitTxRaw enqueue txAux
    saveTx (txId, txAux)
    return accepted

-- | Construct Tx using multiple secret keys and given list of desired outputs.
prepareMTx
    :: TxMode ssc m
    => (Address -> SafeSigner)
    -> NonEmpty Address
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareMTx hdwSigners addrs outputs addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createMTx utxo hdwSigners outputs addrData

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> SafeSigner
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
submitTx enqueue ss outputs addrData = do
    let ourPk = safeToPublic ss
    utxo <- getOwnUtxoForPk ourPk
    txWSpendings <- eitherToThrow =<< createTx utxo ss outputs addrData
    txWSpendings <$ submitAndSave enqueue (fst txWSpendings)

-- | Construct redemption Tx using redemption secret key and a output address
prepareRedemptionTx
    :: TxMode ssc m
    => RedeemSecretKey
    -> Address
    -> m (TxAux, Address, Coin)
prepareRedemptionTx rsk output = do
    let redeemAddress = makeRedeemAddress $ redeemToPublic rsk
    utxo <- getOwnUtxo redeemAddress
    let addCoin c = unsafeAddCoin c . txOutValue . toaOut
        redeemBalance = foldl' addCoin (mkCoin 0) utxo
        txOuts = one $
            TxOutAux {toaOut = TxOut output redeemBalance}
    when (redeemBalance == mkCoin 0) $ throwM RedemptionDepleted
    txAux <- eitherToThrow =<< createRedemptionTx utxo rsk txOuts
    pure (txAux, redeemAddress, redeemBalance)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> TxAux -> m Bool
submitTxRaw enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    sendTx enqueue txAux

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))
