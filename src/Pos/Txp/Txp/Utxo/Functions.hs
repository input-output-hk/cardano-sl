{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions operating on UTXO.

module Pos.Txp.Txp.Utxo.Functions
       ( verifyTxUtxo
       , applyTxToUtxo
       , applyTxToUtxo'
       , verifyAndApplyTxsToUtxo
       ) where

import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as M
import           Universum

import           Pos.Binary.Types          ()
import           Pos.Crypto                (WithHash (..))
import           Pos.Txp.Txp.Class         (MonadTxp (..), MonadTxpRead (..))
import           Pos.Txp.Txp.Failure       (TxpVerFailure (..))
import           Pos.Types.Coin            (unsafeAddCoin)
import           Pos.Types.Core            (Address, Coin, StakeholderId)
import           Pos.Types.Tx              (VTxGlobalContext (..), VTxLocalContext (..),
                                            verifyTx)
import           Pos.Types.Types           (Tx (..), TxAux, TxDistribution (..), TxId,
                                            TxIn (..), TxOut (..), TxOutAux, TxUndo,
                                            TxWitness, TxsUndo, Utxo, txOutStake)

-- CHECK: @verifyTxUtxo
-- | Verify single Tx using MonadUtxoRead as TxIn resolver.
verifyTxUtxo
    :: (MonadTxpRead m, MonadError TxpVerFailure m)
    => Bool
    -> Bool
    -> TxAux
    -> m TxUndo
verifyTxUtxo verifyAlone verifyVersions txaux = do
    res <- verifyTx verifyAlone verifyVersions VTxGlobalContext utxoGet' txaux
    case res of
        Left errors -> notImplemented
        Right undo  -> pure undo
  where
    utxoGet' x = fmap VTxLocalContext <$> utxoGet x

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: MonadTxp m => WithHash Tx -> TxDistribution -> m ()
applyTxToUtxo tx distr = do
    mapM_ applyInput txInputs
    mapM_ (uncurry applyOutput)
      (zip [0..] (zip txOutputs (getTxDistribution distr)))
  where
    Tx {..} = whData tx
    applyInput = utxoDel
    applyOutput idx (out, ds) = utxoPut (TxIn (whHash tx) idx) (out, ds)

applyTxToUtxo' :: MonadTxp m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, d)) = applyTxToUtxo (WithHash t i) d

-- CHECK: @verifyAndApplyTxs
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually. Use it only on temporary data.
verifyAndApplyTxsToUtxo
    :: forall m. (MonadTxp m, MonadError TxpVerFailure m)
    => Bool
    -> Bool
    -> [(WithHash Tx, TxWitness, TxDistribution)]
    -> m TxsUndo
verifyAndApplyTxsToUtxo verifyAlone verifyVersions txs = do
    res <- fmap reverse <$> foldM applyDo (Right []) txs
    case res of
        Left errors -> notImplemented
        Right undos -> pure undos
  where
    applyDo :: Either [Text] TxsUndo
            -> (WithHash Tx, TxWitness, TxDistribution)
            -> m (Either [Text] TxsUndo)
    applyDo failure@(Left _) _ = pure failure
    applyDo (Right txouts) txa = do
        verRes <- verifyTxUtxo verifyAlone verifyVersions (over _1 whData txa)
        (Right $ verRes:txouts) <$ applyTxToUtxo (txa ^. _1) (txa ^. _3)

-- TODO change types of normalizeTxs and related
