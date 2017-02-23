{-# LANGUAGE ConstraintKinds #-}

-- | Logic for local processing of transactions.
-- Local transaction is transaction which hasn't been added in the blockchain yet.

module Pos.Txp.Logic.Local
       ( txProcessTransaction
       , txNormalize
       ) where

import           Control.Monad.Except (MonadError (..), runExcept)
import           Control.Monad.State  (modify')
import           Control.Monad.Trans  (MonadTrans)
import           Data.Default         (def)
import qualified Data.HashMap.Strict  as HM
import           Formatting           (build, sformat, (%))
import           System.Wlog          (WithLogger, logDebug)
import           Universum

import           Pos.DB.Class         (MonadDB)
import qualified Pos.DB.GState        as GS
import           Pos.Types            (Tx (..), TxAux, TxId, TxIn, TxOutAux)

import           Pos.Txp.MemState     (MonadTxpMem (..), getTxpLocalData, getUtxoView,
                                       modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Txp          (MemPool (..), MonadTxPool (..), MonadUtxo (..),
                                       MonadUtxoRead (..), TxpModifier (..),
                                       TxpVerFailure (..), execTxpTLocal, normalizeTxp,
                                       processTx, runDBTxp, runTxpTLocal, utxoGet)


type TxpLocalWorkMode ssc m =
    ( MonadDB ssc m
    , MonadTxpMem m
    , WithLogger m
    , MonadError TxpVerFailure m
    )

-- CHECK: @processTx
-- #processTxDo
txProcessTransaction
    :: TxpLocalWorkMode ssc m
    => (TxId, TxAux) -> m ()
txProcessTransaction itw@(txId, (Tx{..}, _, _)) = do
    tipBefore <- GS.getTip
    localUV <- getUtxoView
    (resolvedOuts, _) <- runDBTxp $ runUV localUV $ mapM utxoGet txInputs
    -- Resolved are transaction outputs which haven't been deleted from the utxo yet
    -- (from Utxo DB and from UtxoView also)
    let resolved = HM.fromList $
                   catMaybes $
                   zipWith (liftM2 (,) . Just) txInputs resolvedOuts
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
                      execTxpTLocal uv mp undo $
                      evalDeepUtxoT resolved $
                      processTx tx in
            case res of
                Left er  -> (Left er, txld)
                Right TxpModifier{..} ->
                    (Right (), (_txmUtxoView, _txmMemPool, _txmUndos, tip))
    runUV uv = runTxpTLocal uv def mempty

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
txNormalize
    :: (MonadDB ssc m, MonadTxpMem m) => m ()
txNormalize = do
    (_, MemPool{..}, _, tip) <- getTxpLocalData
    res <- runExceptT $
           runDBTxp $
           execTxpTLocal def def def $
           normalizeTxp $ HM.toList _mpLocalTxs
    case res of
        Left _                -> setTxpLocalData (def, def, def, tip)
        Right TxpModifier{..} -> setTxpLocalData (_txmUtxoView, _txmMemPool, _txmUndos, tip)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Transformer which holds resolved outputs (see above) and
-- and forwards all Utxo operations to inner monad.

newtype DeepUtxoT m a = DeepUtxoT (StateT (HashMap TxIn TxOutAux) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadError e)

instance MonadTxPool m => MonadTxPool (DeepUtxoT m) where

instance MonadUtxoRead m => MonadUtxoRead (DeepUtxoT m) where
    utxoGet id = DeepUtxoT $ do
        res <- gets $ HM.lookup id
        case res of
            Nothing -> utxoGet id
            Just _  -> pure res

instance MonadUtxo m => MonadUtxo (DeepUtxoT m) where
    utxoPut id aux = DeepUtxoT $ utxoPut id aux
    utxoDel id = DeepUtxoT $ do
        modify' $ HM.delete id
        utxoDel id

evalDeepUtxoT :: Monad m => HashMap TxIn TxOutAux -> DeepUtxoT m a -> m a
evalDeepUtxoT mp (DeepUtxoT st) = evalStateT st mp
