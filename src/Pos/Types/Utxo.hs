{-# LANGUAGE TupleSections #-}
-- | Utxo related operations.

module Pos.Types.Utxo
       ( applyTxToUtxo
       , deleteTxIn
       , findTxIn
       , verifyTxUtxo
       , verifyAndApplyTxs
       ) where

import qualified Data.Map.Strict as M
import           Serokell.Util   (VerificationRes (..))
import           Universum

import           Pos.Crypto      (hash)
import           Pos.Types.Tx    (topsortTxs, verifyTx)
import           Pos.Types.Types (Tx (..), TxIn (..), TxOut (..), Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- | Verify single Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> Tx -> VerificationRes
verifyTxUtxo utxo = verifyTx (`findTxIn` utxo)

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: Tx -> Utxo -> Utxo
applyTxToUtxo tx@Tx {..} =
    foldl' (.) identity
        (map applyInput txInputs ++ zipWith applyOutput [0..] txOutputs)
  where
    h = hash tx
    applyInput txIn = deleteTxIn txIn
    applyOutput idx txOut = M.insert (h, idx) txOut

-- | Accepts list of transactions and verifies its overall properties
-- plus validity of every transaction in particular. Return value is
-- verification failure (first) or topsorted list of transactions (if
-- topsort succeeded -- no loops were found) plus new
-- utxo. @VerificationRes@ is not used here because it can't be
-- applied -- no more than one error can happen. Either transactions
-- can't be topsorted at all or the first incorrect transaction is
-- encountered so we can't proceed further.
verifyAndApplyTxs :: [Tx] -> Utxo -> Either Text ([Tx], Utxo)
verifyAndApplyTxs txs utxo =
    maybe
        (Left "Topsort on transactions failed -- topology is broken")
        (\txs' -> (txs',) <$> applyAll txs')
        topsorted
  where
    applyAll :: [Tx] -> Either Text Utxo
    applyAll [] = Right utxo
    applyAll (tx:xs) = do
        curUtxo <- applyAll xs
        case verifyTxUtxo curUtxo tx of
            VerSuccess          -> pure $ applyTxToUtxo tx curUtxo
            (VerFailure reason) ->
                Left $ fromMaybe "Transaction application failed, reason not specified" $
                head reason
    topsorted = reverse <$> topsortTxs txs -- head is the last one to check
