{-# LANGUAGE FlexibleContexts #-}

-- | Functions for operating with transactions

module Pos.Wallet.Tx
       ( makePubKeyTx
       , submitTx
       , submitTxRaw
       , createTx
       ) where

import           Control.Lens          ((^.), _1)
import           Control.Monad         (fail)
import           Control.TimeWarp.Rpc  (NetworkAddress)
import           Formatting            (build, sformat, (%))
import           System.Wlog           (logError, logInfo)
import           Universum

import           Pos.Binary            ()
import           Pos.Communication     (sendTx)
import           Pos.Crypto            (SecretKey, hash, toPublic)
import           Pos.Types             (Tx, TxAux, TxOut, makePubKeyAddress, txaF)
import           Pos.WorkMode          (MinWorkMode)

import           Pos.Wallet.Tx.Pure    (createTx, makePubKeyTx)
import           Pos.Wallet.WalletMode (TxMode, getOwnUtxo)

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => SecretKey
    -> [NetworkAddress]
    -> [TxOut]
    -> m TxAux
submitTx _ [] _ = logError "No addresses to send" >> fail "submitTx failed"
submitTx sk na outputs = do
    utxo <- getOwnUtxo $ makePubKeyAddress $ toPublic sk
    case createTx utxo sk outputs of
        Left err -> fail $ toString err
        Right tx -> tx <$ submitTxRaw na tx

-- | Send the ready-to-use transaction
submitTxRaw :: MinWorkMode ss m => [NetworkAddress] -> TxAux -> m ()
submitTxRaw na tx = do
    let txId = hash (tx ^. _1)
    logInfo $ sformat ("Submitting transaction: "%txaF) tx
    logInfo $ sformat ("Transaction id: "%build) txId
    mapM_ (`sendTx` tx) na
