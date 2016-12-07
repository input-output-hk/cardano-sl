{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       ) where

import           Control.TimeWarp.Rpc        (BinaryP, Message, MonadDialog)
import           Data.Binary                 (Binary)
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe                  (fromJust)
import           Formatting                  (build, sformat, stext, (%))
import           System.Wlog                 (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Communication.Methods   (announceTxs)
import           Pos.Communication.Types     (ResponseMode)
import           Pos.Crypto                  (WithHash (..), withHash, hash)
import           Pos.DHT                     (ListenerDHT (..), replyToNode)
import           Pos.State                   (ProcessTxRes (..))
import qualified Pos.State                   as St
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.LocalData           (txLocalDataProcessTx)
import           Pos.Txp.LocalData           (getLocalTxs)
import           Pos.Txp.Types.Communication (SendTx (..), SendTxs (..), TxDataMsg (..),
                                              TxInvMsg (..), TxReqMsg (..))
import           Pos.Types                   (IdTxWitness, Tx, TxId, TxWitness,
                                              topsortTxs)
import           Pos.WorkMode                (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
            => [ListenerDHT m]
txListeners =
    [
      ListenerDHT (void . handleTx)
    , ListenerDHT handleTxs
    , ListenerDHT handleTxInv
    , ListenerDHT handleTxReq
    , ListenerDHT handleTxData
    ]

handleTx
    :: ResponseMode ssc m
    => SendTx -> m Bool
handleTx (SendTx tx tw) = do
    added <- handleTxDo $ (hash tx, (tx, tw))
    when added $ do
        notImplemented
    return added

handleTxDo
    :: ResponseMode ssc m
    => IdTxWitness -> m Bool
handleTxDo tx = do
    res <- processTx tx
    let txId = fst tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%build%" failed to verify: "%stext) txId msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) txId
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) txId
    return (res == PTRadded)

handleTxs
    :: (ResponseMode ssc m)
    => SendTxs -> m ()
handleTxs (SendTxs txsUnsorted_) =
    case topsortTxs (\(i, (t, _)) -> WithHash t i) $ NE.toList txsUnsorted of
        Nothing ->
            logWarning "Received broken set of transactions, can't be sorted"
        Just txs -> do
            added <- toList <$> mapM handleTxDo txs
            let addedItems = map (snd . snd) . filter fst . zip added . toList $ txs
            announceTxs addedItems --should we use TxInvMessage here?
  where
    txsUnsorted = fmap (\(t, w) -> (hash t, (t, w))) txsUnsorted_

isTxUsefull :: TxId -> m Bool
isTxUsefull = notImplemented

handleTxInv :: (ResponseMode ssc m)
            => TxInvMsg -> m ()
handleTxInv (TxInvMsg txHashes_) = do
    let txHashes = NE.toList txHashes_
    added <- mapM handleSingle txHashes
    let addedItems = map snd . filter fst . zip added $ txHashes
    safeReply addedItems TxReqMsg
  where
    safeReply :: (Binary r, Message r, ResponseMode ssc m)
              => [a] -> (NonEmpty a -> r) -> m ()
    safeReply [] _      = pure ()
    safeReply xs constr = replyToNode . constr . NE.fromList $ xs
    handleSingle txHash =
        ifM (isTxUsefull txHash)
            (return True)
            (False <$ ingoringLogMsg txHash)
    ingoringLogMsg txHash = logDebug $
        sformat ("Ignoring tx with hash ("%build%"), because it's useless") txHash

handleTxReq :: (ResponseMode ssc m)
            => TxReqMsg -> m ()
handleTxReq (TxReqMsg txIds_) = do
    localTxs <- getLocalTxs
    let txIds = NE.toList txIds_
    found <- mapM lookupSingle txIds
    let addedItems = map (fmap fromJust) . filter (isJust . snd) . zip txIds $ found
    mapM_ (replyToNode . TxDataMsg) addedItems
  where
    lookupSingle :: TxId -> m (Maybe (Tx, TxWitness))
    lookupSingle = notImplemented

handleTxData :: (ResponseMode ssc m)
             => TxDataMsg -> m ()
handleTxData (TxDataMsg tx) =
    --TODO should we check that hash of transaction == txId?
    ifM ((== PTRadded) <$> processTx tx)
        (logDebug $
         sformat ("Tx with hash ("%build%") have been added") (fst tx))
        (logDebug $
         sformat ("Tx with hash ("%build%") have been ignored") (fst tx))

processTx :: ResponseMode ssc m => IdTxWitness -> m ProcessTxRes
processTx tx = do
    utxo <- St.getUtxo
    locRes <- txLocalDataProcessTx tx utxo
    case locRes of
        PTRadded -> PTRadded <$ St.processTx tx
        r        -> return r
