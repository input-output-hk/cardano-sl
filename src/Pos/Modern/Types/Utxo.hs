{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
-- | Utxo related operations.

module Pos.Modern.Types.Utxo
       ( applyTxToUtxo
       , verifyTxUtxo
       , verifyAndApplyTxs
       , applyTxToUtxo'
       ) where

import           Control.Lens         (over, _1)
import           Serokell.Util        (VerificationRes (..))
import           Universum

import           Pos.Binary.Types     ()
import           Pos.Crypto           (WithHash (..))
import           Pos.Modern.Types.Tx  (topsortTxs, verifyTx)
import           Pos.Types.Types      (IdTxWitness, Tx (..), TxIn (..), TxOut (..),
                                       TxWitness)
import           Pos.Types.Utxo.Class (MonadUtxo (..), MonadUtxoRead (..))

-- | Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyAndApplyTxs
    :: MonadUtxo m
    => [(WithHash Tx, TxWitness)]
    -> m (Either Text [(WithHash Tx, TxWitness)])
verifyAndApplyTxs txws  = do
    let
        -- head is the last one to check
        topsorted = reverse <$> topsortTxs fst txws

        applyAll [] = return VerSuccess
        applyAll (txw:xs) = do
            pure (<>)
            <*> (applyAll xs)
            <*> ( do
                  resCur <- verifyTxUtxo (over _1 whData txw)
                  case resCur of
                      VerSuccess ->
                          VerSuccess <$ applyTxToUtxo (fst txw)
                      VerFailure reason ->
                          return $
                              VerFailure $
                                  fromMaybe ["Transaction application failed, reason not specified"] $
                                            (:[]) <$> head reason
                )
    maybe
        (return $ Left "Topsort on transactions failed -- topology is broken")
        (\txs' -> do
            res <- applyAll txs'
            return $
                case res of
                    VerSuccess        -> Right txs'
                    VerFailure reason ->
                        Left $
                            fromMaybe "Transaction application failed, reason not specified" $
                                       head reason
        )
        topsorted

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

-- | Verify single Tx using Utxo as TxIn resolver.
verifyTxUtxo :: MonadUtxoRead m => (Tx, TxWitness) -> m VerificationRes
verifyTxUtxo = verifyTx utxoGet

applyTxToUtxo' :: MonadUtxo m => IdTxWitness -> m ()
applyTxToUtxo' (i, (t, _)) = applyTxToUtxo $ WithHash t i
