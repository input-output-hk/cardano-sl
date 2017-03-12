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
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as M (fromList)
import           Formatting           (build, sformat, (%))
import           System.Wlog          (WithLogger, logDebug)
import           Universum

import           Pos.DB.Class         (MonadDB)
import qualified Pos.DB.GState        as GS
import           Pos.Txp.Core.Types   (Tx (..), TxAux, TxId)

import           Pos.Txp.MemState     (MonadTxpMem (..), getMemPool, getUtxoModifier,
                                       modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil         (MemPool (..), MonadUtxoRead (..), TxpModifier (..),
                                       TxpVerFailure (..), execTxpTLocal, normalizeTxp,
                                       processTx, runDBTxp, runTxpTLocal, runUtxoReaderT,
                                       utxoGet)
#ifdef WITH_EXPLORER
import           Pos.Txp.Toil         (Utxo)
import           Pos.Types.Explorer   (TxExtra (..))
#endif

type TxpLocalWorkMode m =
    ( MonadDB m
    , MonadTxpMem m
    , WithLogger m
    , MonadError TxpVerFailure m
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, (UnsafeTx{..}, _, _)) = do
    tipBefore <- GS.getTip
    localUM <- getUtxoModifier
    (resolvedOuts, _) <- runDBTxp $ runUM localUM $ mapM utxoGet _txInputs
    -- Resolved are transaction outputs which haven't been deleted from the utxo yet
    -- (from Utxo DB and from UtxoView also)
    let resolved = M.fromList $
                   catMaybes $
                   toList $
                   NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    pRes <- modifyTxpLocalData $ processTxDo resolved tipBefore itw
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: "%build) txId
            throwError er
        Right _   ->
            logDebug (sformat ("Transaction is processed successfully: "%build) txId)
  where
    processTxDo resolved tipBefore tx txld@(uv, mp, undo, tip)
        | tipBefore /= tip = (Left $ TxpInvalid "Tips aren't same", txld)
        | otherwise =
            let res = runExcept $
                      flip runUtxoReaderT resolved $
                      execTxpTLocal uv mp undo $
#ifdef WITH_EXPLORER
                      processTx tx $ makeExtra resolved
#else
                      processTx tx
#endif
            in
            case res of
                Left er  -> (Left er, txld)
                Right TxpModifier{..} ->
                    (Right (), (_txmUtxoModifier, _txmMemPool, _txmUndos, tip))
    runUM um = runTxpTLocal um def mempty
#ifdef WITH_EXPLORER
    makeExtra :: Utxo -> TxExtra
    makeExtra resolved = TxExtra Nothing $ NE.fromList $ toList resolved
#endif

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: (MonadDB m, MonadTxpMem m) => m ()
txNormalize = do
    utxoTip <- GS.getTip
    MemPool {..} <- getMemPool
    res <- runExceptT $
           runDBTxp $
           execTxpTLocal mempty def mempty $
           normalizeTxp $
#ifdef WITH_EXPLORER
           HM.toList $ HM.intersectionWith (,) _mpLocalTxs _mpLocalTxsExtra
#else
           HM.toList _mpLocalTxs
#endif
    case res of
        Left _                -> setTxpLocalData (mempty, def, mempty, utxoTip)
        Right TxpModifier{..} -> setTxpLocalData (_txmUtxoModifier, _txmMemPool, _txmUndos, utxoTip)
