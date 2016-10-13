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
import           Pos.Types.Types (AddrId, Address, Coin, Tx (..), TxIn (..), TxOut (..),
                                  Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: Utxo -> TxIn -> Maybe (Address, Coin)
findTxIn utxo TxIn {..} =
    M.lookupLE (txInHash, txInIndex, maxBound) utxo >>= convertRes
  where
    convertRes :: (AddrId, Address) -> Maybe (Address, Coin)
    convertRes ((txId, idx, c), addr)
        | txInHash == txId && txInIndex == idx = pure (addr, c)
        | otherwise = Nothing

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn txIn@TxIn {..} utxo =
    case findTxIn utxo txIn of
        Nothing     -> utxo
        Just (_, c) -> M.delete (txInHash, txInIndex, c) utxo

-- | Verify Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> Tx -> VerificationRes
verifyTxUtxo utxo = verifyTx (findTxIn utxo)

-- | Remove unspent outputs used in given transaction, add new unspent outputs.
applyTxToUtxo :: Tx -> Utxo -> Utxo
applyTxToUtxo tx@Tx {..} =
    foldl' (.) identity
        (map applyInput txInputs ++ map applyOutput (zip [0 ..] txOutputs))
  where
    h = hash tx
    applyInput txIn = deleteTxIn txIn
    applyOutput (idx, TxOut {..}) = M.insert (h, idx, txOutValue) txOutAddress
