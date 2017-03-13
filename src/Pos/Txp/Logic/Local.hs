{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txNormalize
       ) where

import           Control.Monad.Except (MonadError (..), runExcept)
import           Data.Default         (def)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M (fromList)
import           Formatting           (build, sformat, (%))
import           System.Wlog          (WithLogger, logDebug)
import           Universum

import           Pos.DB.Class         (MonadDB)
import qualified Pos.DB.GState        as GS
import           Pos.Txp.Core         (Tx (..), TxAux, TxId)
#ifndef WITH_EXPLORER
import           Pos.Txp.MemState     (getLocalTxs)
#endif
import           Pos.Txp.MemState     (MonadTxpMem (..), getUtxoModifier,
                                       modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil         (MonadUtxoRead (..), ToilModifier (..),
                                       ToilVerFailure (..), execToilTLocal, normalizeToil,
                                       processTx, runDBTxp, runToilTLocal, runUtxoReaderT,
                                       utxoGet)
#ifdef WITH_EXPLORER
import qualified Data.HashMap.Strict  as HM

import           Pos.Txp.MemState     (getMemPool)
import qualified Pos.Util.Modifier    as MM
import           Pos.Slotting         (MonadSlots (currentTimeSlotting))
import           Pos.Txp.Toil         (Utxo, MemPool (..))
import           Pos.Types            (TxExtra (..), Timestamp)
#endif

type TxpLocalWorkMode m =
    ( MonadDB m
    , MonadTxpMem m
    , WithLogger m
    , MonadError ToilVerFailure m
#ifdef WITH_EXPLORER
    , MonadSlots m
#endif
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, (UnsafeTx{..}, _, _)) = do
    tipBefore <- GS.getTip
    localUM <- getUtxoModifier
    -- Note: snapshot isn't used here, because it's not necessary.  If
    -- tip changes after 'getTip' and before resolving all inputs, it's
    -- possible that invalid transaction will appear in
    -- mempool. However, in this case it will be removed by
    -- normalization before releasing lock on block application.
    (resolvedOuts, _) <- runDBTxp $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are unspent transaction outputs corresponding to input
    -- of given transaction.
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
#ifdef WITH_EXPLORER
    curTime <- currentTimeSlotting
#endif
    pRes <- modifyTxpLocalData $
            processTxDo resolved tipBefore itw
#ifdef WITH_EXPLORER
            curTime
#endif
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
#ifdef WITH_EXPLORER
    processTxDo resolved tipBefore tx curTime txld@(uv, mp, undo, tip)
#else
    processTxDo resolved tipBefore tx txld@(uv, mp, undo, tip)
#endif
        | tipBefore /= tip = (Left $ ToilTipsMismatch tipBefore tip, txld)
        | otherwise =
            let res = runExcept $
                      flip runUtxoReaderT resolved $
                      execToilTLocal uv mp undo $
#ifdef WITH_EXPLORER
                      processTx tx $ makeExtra resolved curTime
#else
                      processTx tx
#endif
            in
            case res of
                Left er  -> (Left er, txld)
                Right ToilModifier{..} ->
                    (Right (), (_tmUtxo, _tmMemPool, _tmUndos, tip))
    runUM um = runToilTLocal um def mempty
#ifdef WITH_EXPLORER
    makeExtra :: Utxo -> Timestamp -> TxExtra
    -- NE.fromList is safe here, because if `resolved` is empty, `processTx`
    -- wouldn't save extra value, thus wouldn't reduce it to NF
    makeExtra resolved curTime = TxExtra Nothing curTime $ NE.fromList $ toList resolved
#endif

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: (MonadDB m, MonadTxpMem m) => m ()
txNormalize = do
    utxoTip <- GS.getTip
#ifdef WITH_EXPLORER
    MemPool {..} <- getMemPool
    let localTxs = HM.toList $ HM.intersectionWith (,) _mpLocalTxs $
          MM.insertionsMap _mpLocalTxsExtra
#else
    localTxs <- getLocalTxs
#endif
    ToilModifier {..} <-
        runDBTxp $ execToilTLocal mempty def mempty $ normalizeToil localTxs
    setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip)
