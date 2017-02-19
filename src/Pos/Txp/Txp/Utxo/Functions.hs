{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions operating on UTXO.

module Pos.Txp.Txp.Utxo.Functions
       ( verifyTxUtxo
       , applyTxToUtxo
       , applyTxToUtxo'
       , rollbackTxUtxo
       ) where

import           Control.Monad.Error.Class (MonadError (..))
import qualified Data.Text                 as T
import           Universum

import           Pos.Binary.Types          ()
import           Pos.Crypto                (WithHash (..), hash)
import           Pos.Types.Tx              (VTxGlobalContext (..), VTxLocalContext (..),
                                            verifyTx)
import           Pos.Types.Types           (Tx (..), TxAux, TxDistribution (..), TxId,
                                            TxIn (..), TxUndo)

import           Pos.Txp.Txp.Class         (MonadTxp (..), MonadTxpRead (..))
import           Pos.Txp.Txp.Failure       (TxpVerFailure (..))

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
        Left errors -> throwError $ TxpInvalid $ T.intercalate "; " errors
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

rollbackTxUtxo
    :: (MonadError TxpVerFailure m, MonadTxp m)
    => (TxAux, TxUndo) -> m ()
rollbackTxUtxo ((tx@Tx{..}, _, _), undo) = do
    unless (length txInputs == length undo) $
        throwError $ TxpInvalidUndoLength (length txInputs) (length undo)
    let txid = hash tx
    mapM_ utxoDel $ take (length txOutputs) $ zipWith TxIn (repeat txid) [0..]
    mapM_ (uncurry utxoPut) $ zip txInputs undo

applyTxToUtxo' :: MonadTxp m => (TxId, TxAux) -> m ()
applyTxToUtxo' (i, (t, _, d)) = applyTxToUtxo (WithHash t i) d

-- TODO change types of normalizeTxs and related
