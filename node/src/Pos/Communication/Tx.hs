{-# LANGUAGE RankNTypes #-}

-- | Functions for operating with transactions

module Pos.Communication.Tx
       ( TxMode
       , submitTx
       , submitMTx
       , submitRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Client.Txp.Addresses   (MonadAddresses (..))
import           Pos.Client.Txp.Balances    (MonadBalances (..), getOwnUtxo)
import           Pos.Client.Txp.History     (MonadTxHistory (..))
import           Pos.Client.Txp.Util        (TxCreateMode, TxError (..), createMTx,
                                             createRedemptionTx, createTx)
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (EnqueueMsg, OutSpecs)
import           Pos.Communication.Specs    (createOutSpecs)
import           Pos.Communication.Types    (InvOrDataTK)
import           Pos.Core                   (Address, Coin, IsBootstrapEraAddr (..),
                                             makePubKeyAddress, makeRedeemAddress, mkCoin,
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
    => EnqueueMsg m -> TxAux -> m TxAux
submitAndSave enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    submitTxRaw enqueue txAux
    saveTx (txId, txAux)
    return txAux

-- | Construct Tx using multiple secret keys and given list of desired outputs.
submitMTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> NonEmpty (SafeSigner, Address)
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
submitMTx enqueue hdwSigner outputs addrData = do
    let addrs = map snd $ toList hdwSigner
    utxo <- getOwnUtxos addrs
    txWSpendings <- eitherToThrow =<< createMTx utxo hdwSigner outputs addrData
    txWSpendings <$ submitAndSave enqueue (fst txWSpendings)

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
    -- Here we want to get utxo for all addresses which we «own»,
    -- i. e. can spend funds from them. We can't get all such
    -- addresses, because it's impossible to extract spending data
    -- from an address. And we can't enumerate all possible addresses
    -- for a public key. So we only consider two addresses: one with
    -- bootstrap era distribution and another one with signle key
    -- distribution.
    let ourAddresses :: [Address]
        ourAddresses =
            map (flip makePubKeyAddress ourPk . IsBootstrapEraAddr) [False, True]
    utxo <- getOwnUtxos ourAddresses
    txWSpendings <- eitherToThrow =<< createTx utxo ss outputs addrData
    txWSpendings <$ submitAndSave enqueue (fst txWSpendings)

-- | Construct redemption Tx using redemption secret key and a output address
submitRedemptionTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> RedeemSecretKey
    -> Address
    -> m (TxAux, Address, Coin)
submitRedemptionTx enqueue rsk output = do
    let redeemAddress = makeRedeemAddress $ redeemToPublic rsk
    utxo <- getOwnUtxo redeemAddress
    let addCoin c = unsafeAddCoin c . txOutValue . toaOut
        redeemBalance = foldl' addCoin (mkCoin 0) utxo
        txOuts = one $
            TxOutAux {toaOut = TxOut output redeemBalance}
    when (redeemBalance == mkCoin 0) $ throwM . TxError $ "Redeem balance is 0"
    txw <- eitherToThrow =<< createRedemptionTx utxo rsk txOuts
    txAux <- submitAndSave enqueue txw
    pure (txAux, redeemAddress, redeemBalance)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> TxAux -> m ()
submitTxRaw enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    sendTx enqueue txAux

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))
