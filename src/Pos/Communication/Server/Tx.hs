{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Rpc      (BinaryP, MonadDialog)
import qualified Data.List.NonEmpty        as NE (toList)
import           Formatting                (build, sformat, stext, (%))
import           System.Wlog               (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Communication.Methods (announceTxs)
import           Pos.Communication.Types   (ResponseMode, SendTx (..), SendTxs (..))
import           Pos.Crypto                (WithHash (whData), withHash)
import           Pos.DHT                   (ListenerDHT (..))
import           Pos.State                 (ProcessTxRes (..), processTx)
import           Pos.Statistics            (StatProcessTx (..), statlogCountEvent)
import           Pos.Types                 (Tx, topsortTxs)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog BinaryP m, WorkMode ssc m)
            => [ListenerDHT m]
txListeners = [ListenerDHT (void . handleTx), ListenerDHT handleTxs]

handleTx
    :: ResponseMode ssc m
    => SendTx -> m Bool
handleTx (SendTx tx) = handleTxDo $ withHash tx

handleTxDo
    :: ResponseMode ssc m
    => WithHash Tx -> m Bool
handleTxDo tx = do
    res <- processTx tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) tx
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%build%" failed to verify: "%stext) tx msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) tx
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) tx
    return (res == PTRadded)

handleTxs
    :: (ResponseMode ssc m)
    => SendTxs -> m ()
handleTxs (SendTxs txsUnsorted_) =
    case topsortTxs $ NE.toList txsUnsorted of
        Nothing ->
            logWarning "Received broken set of transactions, can't be sorted"
        Just txs -> do
            added <- toList <$> mapM handleTxDo txs
            let addedItems = map (whData . snd) . filter fst . zip added . toList $ txs
            announceTxs addedItems
  where
    txsUnsorted = fmap withHash txsUnsorted_
