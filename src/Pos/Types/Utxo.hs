-- | Utxo related operations.

module Pos.Types.Utxo
       ( findTxIn
       , verifyTxUtxo
       ) where

import qualified Data.Map.Strict as M
import           Serokell.Util   (VerificationRes)
import           Universum

import           Pos.Types.Tx    (verifyTx)
import           Pos.Types.Types (AddrId, Address, Coin, Tx, TxIn (..), Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: Utxo -> TxIn -> Maybe (Address, Coin)
findTxIn utxo TxIn {..} =
    M.lookupLE (txInHash, txInIndex, maxBound) utxo >>= convertRes
  where
    convertRes :: (AddrId, Address) -> Maybe (Address, Coin)
    convertRes ((txId, idx, c), addr)
        | txInHash == txId && txInIndex == idx = pure (addr, c)
        | otherwise = Nothing

-- | Verify Tx using Utxo as TxIn resolver.
verifyTxUtxo :: Utxo -> Tx -> VerificationRes
verifyTxUtxo utxo = verifyTx (findTxIn utxo)
