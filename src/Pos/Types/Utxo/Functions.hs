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
       , applyTxToUtxo'
       , convertTo'
       , convertFrom'
       , belongsTo
       , filterUtxoByAddr
       ) where

import           Control.Lens         (over, (^.), _1, _3)
import qualified Data.Map.Strict      as M
import           Universum

import           Pos.Binary.Types     ()
import           Pos.Crypto           (WithHash (..))
import           Pos.Types.Tx         (VTxGlobalContext (..), VTxLocalContext (..),
                                       verifyTx)
import           Pos.Types.Types      (Address, Tx (..), TxAux, TxDistribution (..), TxId,
                                       TxIn (..), TxOut (..), TxOutAux, TxWitness, Undo,
                                       Utxo)
import           Pos.Types.Utxo.Class (MonadUtxo (..), MonadUtxoRead (utxoGet))

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOutAux
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- CHECK: @verifyTxUtxo
-- | Verify single Tx using MonadUtxoRead as TxIn resolver.
verifyTxUtxo :: MonadUtxoRead m => Bool -> TxAux -> m (Either Text [TxOutAux])
verifyTxUtxo verifyAlone = verifyTx verifyAlone VTxGlobalContext utxoGet'
  where
    utxoGet' x = fmap VTxLocalContext <$> utxoGet x

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: MonadUtxo m => WithHash Tx -> TxDistribution -> m ()
applyTxToUtxo tx distr = do
    mapM_ applyInput txInputs
    mapM_ (uncurry applyOutput)
      (zip [0..] (zip txOutputs (getTxDistribution distr)))
  where
    Tx {..} = whData tx
    applyInput = utxoDel
    applyOutput idx (out, ds) = utxoPut (TxIn (whHash tx) idx) (out, ds)

applyTxToUtxo' :: MonadUtxo m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, d)) = applyTxToUtxo (WithHash t i) d

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
    applyDo txouts txa = do
        verRes <- verifyTxUtxo verifyAlone (over _1 whData txa)
        ((:) <$> verRes <*> txouts) <$ applyTxToUtxo (txa ^. _1) (txa ^. _3)

-- TODO change types of normalizeTxs and related

convertTo' :: [(TxId, TxAux)] -> [(WithHash Tx, TxWitness, TxDistribution)]
convertTo' = map (\(i, (t, w, d)) -> (WithHash t i, w, d))

convertFrom' :: [(WithHash Tx, TxWitness, TxDistribution)] -> [(TxId, TxAux)]
convertFrom' = map (\(WithHash t h, w, d) -> (h, (t, w, d)))

-- | A predicate for `TxOut` which selects outputs for given address
belongsTo :: TxOutAux -> Address -> Bool
(out, _) `belongsTo` addr = addr == txOutAddress out

-- | Select only TxOuts for given addresses
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`belongsTo` addr)
