{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Functions operating on UTXO.

module Pos.Types.Utxo.Functions
       ( applyTxToUtxo
       , deleteTxIn
       , findTxIn
       , verifyTxUtxo
       , verifyAndApplyTxs
       , verifyAndApplyTxsOld
       , applyTxToUtxo'
       , verifyAndApplyTxsOld'
       , convertTo'
       , convertFrom'
       , belongsTo
       , filterUtxoByAddr
       ) where

import           Control.Lens         (over, view, (^.), _1)
import           Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import qualified Data.Map.Strict      as M
import           Universum

import           Pos.Binary.Types     ()
import           Pos.Crypto           (WithHash (..))
import           Pos.Types.Tx         (VTxGlobalContext (..), VTxLocalContext (..),
                                       topsortTxs, verifyTx)
import           Pos.Types.Types      (Address, Tx (..), TxAux, TxDistribution, TxId,
                                       TxIn (..), TxOut (..), TxWitness, Undo, Utxo)
import           Pos.Types.Utxo.Class (MonadUtxo (..), MonadUtxoRead (utxoGet))

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- CHECK: @verifyTxUtxo
-- | Verify single Tx using MonadUtxoRead as TxIn resolver.
verifyTxUtxo :: MonadUtxoRead m => Bool -> TxAux -> m (Either Text [TxOut])
verifyTxUtxo verifyAlone = verifyTx verifyAlone VTxGlobalContext utxoGet'
  where
    utxoGet' x = fmap VTxLocalContext <$> utxoGet x

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: MonadUtxo m => WithHash Tx -> m ()
applyTxToUtxo tx = do
    mapM_ applyInput txInputs
    mapM_ (uncurry applyOutput) (zip [0..] txOutputs)
  where
    Tx {..} = whData tx
    applyInput = utxoDel
    applyOutput idx = utxoPut $ TxIn (whHash tx) idx

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, _)) = applyTxToUtxo $ WithHash t i

-- CHECK: @verifyAndApplyTxs
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyAndApplyTxs
    :: forall m.
       MonadUtxo m
    => Bool
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> m (Either Text Undo)
verifyAndApplyTxs verifyAlone txs = fmap reverse <$> foldM applyDo (Right []) txs
  where
    applyDo :: Either Text Undo
            -> (WithHash Tx, TxWitness, TxDistribution)
            -> m (Either Text Undo)
    applyDo failure@(Left _) _ = pure failure
    applyDo txouts txw = do
        verRes <- verifyTxUtxo verifyAlone (over _1 whData txw)
        ((:) <$> verRes <*> txouts) <$ applyTxToUtxo (txw ^. _1)

-- CHECK: @verifyAndApplyTxsOld
-- | DEPRECATED
-- Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyAndApplyTxsOld
    :: forall m.
       MonadUtxo m
    => [(WithHash Tx, TxWitness, TxDistribution)]
    -> m (Either Text [(WithHash Tx, TxWitness, TxDistribution)])
verifyAndApplyTxsOld txws =
    runExceptT $
    maybe (throwError brokenMsg) (\txs' -> txs' <$ applyAll txs') topsorted
  where
    brokenMsg = "Topsort on transactions failed -- topology is broken"
    applyAll :: [(WithHash Tx, TxWitness, TxDistribution)]
             -> ExceptT Text m ()
    applyAll [] = pass
    applyAll (txw:xs) = do
        applyAll xs
        () <$ ExceptT (verifyTxUtxo True (over _1 whData txw))
        applyTxToUtxo $ txw ^. _1
     -- 'reverse' because head is the last one to check
    topsorted = reverse <$> topsortTxs (view _1) txws

-- TODO change types of normalizeTxs and related

convertTo' :: [(TxId, TxAux)] -> [(WithHash Tx, TxWitness, TxDistribution)]
convertTo' = map (\(i, (t, w, d)) -> (WithHash t i, w, d))

convertFrom' :: [(WithHash Tx, TxWitness, TxDistribution)] -> [(TxId, TxAux)]
convertFrom' = map (\(WithHash t h, w, d) -> (h, (t, w, d)))

-- CHECK: @verifyAndApplyTxsOld'
-- #verifyAndApplyTxsOld
verifyAndApplyTxsOld'
    :: MonadUtxo m
    => [(TxId, TxAux)] -> m (Either Text [(TxId, TxAux)])
verifyAndApplyTxsOld' txws =
    fmap convertFrom' <$> verifyAndApplyTxsOld (convertTo' txws)

-- | A predicate for `TxOut` which selects outputs for given address
belongsTo :: TxOut -> Address -> Bool
out `belongsTo` addr = addr == txOutAddress out

-- | Select only TxOuts for given addresses
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`belongsTo` addr)
