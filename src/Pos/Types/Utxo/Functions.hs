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
       , verifyAndApplyTxs'
       , convertTo'
       , convertFrom'
       ) where

import           Control.Lens         (over, _1)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map.Strict      as M
import           Serokell.Util        (VerificationRes (..))
import           Universum

import           Pos.Binary.Types     ()
import           Pos.Crypto           (WithHash (..))
import           Pos.Types.Tx         (topsortTxs, verifyTx)
import           Pos.Types.Types      (IdTxWitness, Tx (..), TxIn (..), TxOut (..),
                                       TxWitness, Utxo)
import           Pos.Types.Utxo.Class (MonadUtxo (..), MonadUtxoRead (utxoGet))

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- CHECK: @verifyTxUtxo
-- | Verify single Tx using MonadUtxoRead as TxIn resolver.
verifyTxUtxo :: MonadUtxoRead m => (Tx, TxWitness) -> m VerificationRes
verifyTxUtxo = verifyTx utxoGet

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

applyTxToUtxo' :: MonadUtxo m => IdTxWitness -> m ()
applyTxToUtxo' (i, (t, _)) = applyTxToUtxo $ WithHash t i

-- CHECK: @verifyAndApplyTxs
-- | Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyAndApplyTxs
    :: forall m.
       MonadUtxo m
    => [(WithHash Tx, TxWitness)] -> m (Either Text [(WithHash Tx, TxWitness)])
verifyAndApplyTxs txws =
    runExceptT $
    maybe (throwError brokenMsg) (\txs' -> txs' <$ applyAll txs') topsorted
  where
    brokenMsg = "Topsort on transactions failed -- topology is broken"
    applyAll :: [(WithHash Tx, TxWitness)] -> ExceptT Text m ()
    applyAll [] = pass
    applyAll (txw:xs) = do
        applyAll xs
        verRes <- verifyTxUtxo (over _1 whData txw)
        case verRes of
            VerSuccess -> applyTxToUtxo $ fst txw
            VerFailure reason ->
                throwError $
                fromMaybe "Transaction application failed, reason not specified" $
                head reason
    topsorted = reverse <$> topsortTxs fst txws -- head is the last one to check

-- TODO change types of normalizeTxs and related

convertTo' :: [IdTxWitness] -> [(WithHash Tx, TxWitness)]
convertTo' = map (\(i, (t, w)) -> (WithHash t i, w))

convertFrom' :: [(WithHash Tx, TxWitness)] -> [IdTxWitness]
convertFrom' = map (\(WithHash t h, w) -> (h, (t, w)))

-- CHECK: @verifyAndApplyTxs'
-- #verifyAndApplyTxs
verifyAndApplyTxs'
    :: MonadUtxo m
    => [IdTxWitness] -> m (Either Text [IdTxWitness])
verifyAndApplyTxs' txws =
    fmap convertFrom' <$> verifyAndApplyTxs (convertTo' txws)
