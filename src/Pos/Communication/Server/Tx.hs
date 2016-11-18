{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import           Formatting                (build, sformat, stext, (%))
import           System.Wlog               (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.Communication.Types   (ResponseMode, SendTx (..), SendTxs (..))
import           Pos.DHT                   (ListenerDHT (..))
import           Pos.State                 (ProcessTxRes (..), processTx)
import           Pos.Statistics            (StatProcessTx (..), statlogCountEvent)
import           Pos.Types                 (txF)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
            => [ListenerDHT m]
txListeners = [ListenerDHT (void . handleTx), ListenerDHT handleTxs]

handleTx
    :: ResponseMode ssc m
    => SendTx -> m Bool
handleTx (SendTx tx) = do
    res <- processTx tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) tx
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%txF%" failed to verify: "%stext) tx msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) tx
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) tx
    return (res == PTRadded)

handleTxs
    :: (ResponseMode ssc m)
    => SendTxs -> m ()
handleTxs (SendTxs txs) = do
    added <- toList <$> mapM (handleTx . SendTx) txs
    let addedItems = map snd . filter fst . zip added . toList $ txs
    announceTxs addedItems
