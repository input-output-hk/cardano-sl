{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for operating with transactions

module Pos.Client.Txp.Network
       ( TxMode
       , submitTx
       , prepareMTx
       , prepareRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Universum

import           Formatting (build, sformat, (%))
import           Mockable (MonadMockable)
import           System.Wlog (logInfo)

import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..), getOwnUtxo, getOwnUtxoForPk)
import           Pos.Client.Txp.History (MonadTxHistory (..))
import           Pos.Client.Txp.Util (InputSelectionPolicy, PendingAddresses (..), TxCreateMode,
                                      TxError (..), createMTx, createRedemptionTx, createTx)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (OutSpecs)
import           Pos.Communication.Specs (createOutSpecs)
import           Pos.Communication.Types (InvOrDataTK)
import           Pos.Core (Address, Coin, makeRedeemAddress, mkCoin, unsafeAddCoin)
import           Pos.Core.Txp (TxAux (..), TxId, TxOut (..), TxOutAux (..), txaF)
import           Pos.Crypto (RedeemSecretKey, SafeSigner, hash, redeemToPublic, safeToPublic)
import           Pos.DB.Class (MonadGState)
import           Pos.Diffusion.Types (Diffusion (sendTx))
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.Util.Util (eitherToThrow)
import           Pos.WorkMode.Class (MinWorkMode)

type TxMode m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory m
      , MonadMockable m
      , MonadMask m
      , MonadThrow m
      , TxCreateMode m
      )

submitAndSave
    :: TxMode m
    => Diffusion m -> TxAux -> m Bool
submitAndSave diffusion txAux@TxAux {..} = do
    let txId = hash taTx
    accepted <- submitTxRaw diffusion txAux
    saveTx (txId, txAux)
    pure accepted

-- | Construct Tx using multiple secret keys and given list of desired outputs.
prepareMTx
    :: TxMode m
    => (Address -> Maybe SafeSigner)
    -> PendingAddresses
    -> InputSelectionPolicy
    -> NonEmpty Address
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
prepareMTx hdwSigners pendingAddrs inputSelectionPolicy addrs outputs addrData = do
    utxo <- getOwnUtxos (toList addrs)
    eitherToThrow =<< createMTx pendingAddrs inputSelectionPolicy utxo hdwSigners outputs addrData

-- | Construct redemption Tx using redemption secret key and a output address
prepareRedemptionTx
    :: TxMode m
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
    => Diffusion m -> TxAux -> m Bool
submitTxRaw diffusion txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    sendTx diffusion txAux

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))

-- | Construct Tx using secret key and given list of desired outputs
-- BE CAREFUL! Doesn't consider HD wallet addresses
submitTx
    :: TxMode m
    => Diffusion m
    -> PendingAddresses
    -> SafeSigner
    -> NonEmpty TxOutAux
    -> AddrData m
    -> m (TxAux, NonEmpty TxOut)
submitTx diffusion pendingAddrs ss outputs addrData = do
    let ourPk = safeToPublic ss
    utxo <- getOwnUtxoForPk ourPk
    txWSpendings <- eitherToThrow =<< createTx pendingAddrs utxo ss outputs addrData
    txWSpendings <$ submitAndSave diffusion (fst txWSpendings)
