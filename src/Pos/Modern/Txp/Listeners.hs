{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server which handles transactions.

module Pos.Modern.Txp.Listeners
       ( txListeners
       ) where

import           Control.TimeWarp.Rpc (MonadDialog)
-- import qualified Data.HashMap.Strict         as HM
-- import qualified Data.List.NonEmpty          as NE
-- import           Data.Maybe                  (fromJust)
-- import           Formatting                  (build, sformat, stext, (%))
-- import           System.Wlog                 (logDebug, logInfo, logWarning)
-- import           Universum

-- import           Pos.Communication.Methods   (sendToNeighborsSafe)
-- import           Pos.Communication.Types     (ResponseMode)
-- import           Pos.Crypto                  (hash)
import           Pos.DHT              (ListenerDHT (..))
-- import           Pos.State                   (ProcessTxRes (..))
-- import qualified Pos.State                   as St
-- import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
-- import           Pos.Txp.LocalData           (txLocalDataProcessTx)
-- import           Pos.Txp.LocalData           (getLocalTxs)
-- import           Pos.Txp.Types.Communication (TxDataMsg (..), TxInvMsg (..),
--                                               TxReqMsg (..))
-- import           Pos.Types                   (IdTxWitness, TxId)
import           Pos.WorkMode         (UserPacking, WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: (MonadDialog UserPacking m, WorkMode ssc m)
            => [ListenerDHT m]
txListeners =
    [
    --   ListenerDHT handleTxInv
    -- , ListenerDHT handleTxReq
    -- , ListenerDHT handleTxData
    ]

-- handleTxInv :: (ResponseMode ssc m) => TxInvMsg -> m ()
-- handleTxInv (TxInvMsg txHashes_) = do
--     let txHashes = NE.toList txHashes_
--     added <- mapM handleSingle txHashes
--     let addedItems = map snd . filter fst . zip added $ txHashes
--     safeReply addedItems TxReqMsg
--   where
--     safeReply [] _      = pure ()
--     safeReply xs constr = replyToNode . constr . NE.fromList $ xs
--     handleSingle txHash =
--         ifM (isTxUsefull txHash)
--             (return True)
--             (False <$ ingoringLogMsg txHash)
--     ingoringLogMsg txHash = logDebug $
--         sformat ("Ignoring tx with hash ("%build%"), because it's useless") txHash

-- handleTxReq :: (ResponseMode ssc m)
--             => TxReqMsg -> m ()
-- handleTxReq (TxReqMsg txIds_) = do
--     localTxs <- getLocalTxs
--     let txIds = NE.toList txIds_
--         found = map (flip HM.lookup localTxs) txIds
--         addedItems = map fromJust . filter isJust $ found
--     mapM_ (replyToNode . uncurry TxDataMsg) addedItems

-- handleTxData :: (ResponseMode ssc m)
--              => TxDataMsg -> m ()
-- handleTxData (TxDataMsg tx tw) = do
--     let txId = hash tx
--     added <- handleTxDo (txId, (tx, tw))
--     when added $ sendToNeighborsSafe $ TxInvMsg $ pure txId

-- isTxUsefull :: ResponseMode ssc m => TxId -> m Bool
-- isTxUsefull txId = not . HM.member txId <$> getLocalTxs

-- -- Real tx processing
-- handleTxDo
--     :: ResponseMode ssc m
--     => IdTxWitness -> m Bool
-- handleTxDo tx = do
--     res <- processTx tx
--     let txId = fst tx
--     case res of
--         PTRadded -> do
--             statlogCountEvent StatProcessTx 1
--             logInfo $
--                 sformat ("Transaction has been added to storage: "%build) txId
--         PTRinvalid msg ->
--             logWarning $
--             sformat ("Transaction "%build%" failed to verify: "%stext) txId msg
--         PTRknown ->
--             logDebug $ sformat ("Transaction is already known: "%build) txId
--         PTRoverwhelmed ->
--             logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) txId
--     return (res == PTRadded)

-- processTx :: ResponseMode ssc m => IdTxWitness -> m ProcessTxRes
-- processTx tx = do
--     utxo <- St.getUtxo
--     locRes <- txLocalDataProcessTx tx utxo
--     case locRes of
--         PTRadded -> PTRadded <$ St.processTx tx
--         r        -> return r
