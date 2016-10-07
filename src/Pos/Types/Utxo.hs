-- | Utxo related operations.

module Pos.Types.Utxo
       ( findTxIn
       ) where

import qualified Data.Map.Strict as M
import           Universum

import           Pos.Types.Types (AddrId, Address, Coin, TxIn (..), Utxo)

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: Utxo -> TxIn -> Maybe (Address, Coin)
findTxIn utxo TxIn {..} =
    M.lookupLE (txInHash, txInIndex, maxBound) utxo >>= convertRes
  where
    convertRes :: (AddrId, Address) -> Maybe (Address, Coin)
    convertRes ((txId, idx, c), addr)
        | txInHash == txId && txInIndex == idx = pure (addr, c)
        | otherwise = Nothing
