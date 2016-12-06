{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.Lens              ((%~), _1)
import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import qualified Data.List.NonEmpty        as NE (toList)
import           Formatting                (sformat, stext, (%))
import           System.Wlog               (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.Communication.Types   (ResponseMode, SendTx (..), SendTxs (..))
import           Pos.Crypto                (WithHash (whData), withHash)
import           Pos.DHT                   (ListenerDHT (..))
import           Pos.State                 (ProcessTxRes (..), processTx)
import           Pos.Statistics            (StatProcessTx (..), statlogCountEvent)
import           Pos.Types                 (Tx, TxWitness, topsortTxs', txwF)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
            => [ListenerDHT m]
txListeners = [ListenerDHT (void . handleTx), ListenerDHT handleTxs]

handleTx
    :: ResponseMode ssc m
    => SendTx -> m Bool
handleTx (SendTx tx w) = handleTxDo (withHash tx, w)

handleTxDo
    :: ResponseMode ssc m
    => (WithHash Tx, TxWitness) -> m Bool
handleTxDo (tx, w) = do
    res <- processTx (tx, w)
    let txw = (whData tx, w)
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%txwF) txw
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%txwF%" failed to verify: "%stext) txw msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%txwF) txw
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%txwF) txw
    return (res == PTRadded)

handleTxs
    :: (ResponseMode ssc m)
    => SendTxs -> m ()
handleTxs (SendTxs txsUnsorted_) =
    case topsortTxs' fst $ NE.toList txsUnsorted of
        Nothing ->
            logWarning "Received broken set of transactions, can't be sorted"
        Just txws -> do
            added <- toList <$> mapM handleTxDo txws
            let addedItems :: [(Tx, TxWitness)]
                addedItems =
                    map (_1 %~ whData) .                -- remove hashes
                    map snd . filter fst . zip added $  -- leave only added
                      txws
            announceTxs addedItems
  where
    txsUnsorted = fmap (_1 %~ withHash) txsUnsorted_
