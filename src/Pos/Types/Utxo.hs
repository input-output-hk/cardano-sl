module Pos.Types.Utxo
    (
    -- * Pure
      deleteTxIn
    , findTxIn
    , belongsTo
    , filterUtxoByAddr
    , utxoToStakes
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import           Universum


import           Pos.Types.Address   (Address)
import           Pos.Types.Coin      (unsafeAddCoin)
import           Pos.Types.Core      (Coin, StakeholderId)
import           Pos.Types.Types     (Tx (..), TxAux, TxDistribution (..), TxId,
                                      TxIn (..), TxOut (..), TxOutAux, TxUndo, TxWitness,
                                      TxsUndo, Utxo, txOutStake)
----------------------------------------------------------------------------
-- Pure
----------------------------------------------------------------------------

-- | Find transaction input in Utxo assuming it is valid.
findTxIn :: TxIn -> Utxo -> Maybe TxOutAux
findTxIn TxIn{..} = M.lookup (txInHash, txInIndex)

-- | Delete given TxIn from Utxo if any.
deleteTxIn :: TxIn -> Utxo -> Utxo
deleteTxIn TxIn{..} = M.delete (txInHash, txInIndex)

-- | A predicate for `TxOut` which selects outputs for given address
belongsTo :: TxOutAux -> Address -> Bool
(out, _) `belongsTo` addr = addr == txOutAddress out

-- | Select only TxOuts for given addresses
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`belongsTo` addr)

utxoToStakes :: Utxo -> HashMap StakeholderId Coin
utxoToStakes = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddCoin key val hm
    putDistr hm (_, toaux) = foldl' plusAt hm (txOutStake toaux)
