-- | Utxo related operations.

module Pos.Types.Utxo
       ( applyTxToUtxo
       , deleteTxIn
       , findTxIn
       , verifyTxUtxo
       ) where

import qualified Data.Map.Strict as M
import           Serokell.Util   (VerificationRes)
import           Universum

import           Pos.Crypto      (hash)
import           Pos.Types.Tx    (verifyTx)
import           Pos.Types.Types (Tx (..), TxIn (..), TxOut (..), Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOut
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- | Verify Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> Tx -> VerificationRes
verifyTxUtxo utxo = verifyTx (`findTxIn` utxo)

-- | Remove unspent outputs used in given transaction, add new unspent outputs.
applyTxToUtxo :: Tx -> Utxo -> Utxo
applyTxToUtxo tx@Tx {..} =
    foldl' (.) identity
        (map applyInput txInputs ++ zipWith applyOutput [0..] txOutputs)
  where
    h = hash tx
    applyInput txIn = deleteTxIn txIn
    applyOutput idx txOut = M.insert (h, idx) txOut
