{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       ) where

import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import           Formatting                  (build, sformat, stext, (%))
import           System.Wlog                 (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Txp              ()
import           Pos.Communication.Methods   (sendToNeighborsSafe)
import           Pos.Communication.Types     (MutSocketState, ResponseMode)
import           Pos.Context                 (WithNodeContext (getNodeContext),
                                              ncPropagation)
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model               (ListenerDHT (..), MonadDHTDialog,
                                              replyToNode)
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Class               (MonadTxpLD, getMemPool)
import           Pos.Txp.Logic               (processTx)
import           Pos.Txp.Types.Communication (TxDataMsg (..), TxInvMsg (..),
                                              TxReqMsg (..))
import           Pos.Txp.Types.Types         (MemPool (..), ProcessTxRes (..), TxMap)
import           Pos.Types                   (IdTxWitness, TxId)
import           Pos.WorkMode                (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
txListeners =
    [
      ListenerDHT handleTxInv
    , ListenerDHT handleTxReq
    , ListenerDHT handleTxData
    ]

isTxUseful :: ResponseMode ssc m => TxId -> m Bool
isTxUseful txId = not . HM.member txId <$> getLocalTxsMap

handleTxInv :: (ResponseMode ssc m) => TxInvMsg -> m ()
handleTxInv (TxInvMsg (NE.toList -> txHashes)) = do
    added <- mapM handleSingle txHashes
    let addedItems = map snd . filter fst . zip added $ txHashes
    safeReply addedItems TxReqMsg
  where
    safeReply [] _      = pure ()
    safeReply xs constr = replyToNode . constr . NE.fromList $ xs
    handleSingle txHash =
        ifM (isTxUseful txHash)
            (True <$ requestingLogMsg txHash)
            (False <$ ingoringLogMsg txHash)
    requestingLogMsg txHash = logDebug $
        sformat ("Requesting tx with hash "%build) txHash
    ingoringLogMsg txHash = logDebug $
        sformat ("Ignoring tx with hash ("%build%"), because it's useless") txHash

handleTxReq :: (ResponseMode ssc m)
            => TxReqMsg -> m ()
handleTxReq (TxReqMsg txIds_) = do
    localTxs <- getLocalTxsMap
    let txIds = NE.toList txIds_
        found = map (flip HM.lookup localTxs) txIds
        addedItems = catMaybes found
    mapM_ (replyToNode . uncurry TxDataMsg) addedItems

-- CHECK: #handleTxDo
handleTxData :: (ResponseMode ssc m)
             => TxDataMsg -> m ()
handleTxData (TxDataMsg tx tw) = do
    let txId = hash tx
    added <- handleTxDo (txId, (tx, tw))
    needPropagate <- ncPropagation <$> getNodeContext
    when (added && needPropagate) $ sendToNeighborsSafe $ TxInvMsg $ pure txId

-- Real tx processing
-- CHECK: @handleTxDo
-- #processTx
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

getLocalTxsMap :: MonadTxpLD ssc m => m TxMap
getLocalTxsMap = localTxs <$> getMemPool
