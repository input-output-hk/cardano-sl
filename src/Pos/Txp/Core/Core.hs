-- | Helper for core types.

module Pos.Txp.Core.Core
       ( txInToPair
       , txOutStake
       , mkTxProof
       ) where

import           Universum

import           Pos.Binary.Core    ()
import           Pos.Binary.Crypto  ()
import           Pos.Binary.Txp     ()
import           Pos.Core.Address   ()
import           Pos.Core.Types     (Address (..), Coin, StakeholderId)
import           Pos.Crypto         (hash)
import           Pos.Merkle         (mtRoot)
import           Pos.Txp.Core.Types (TxId, TxIn (..), TxOut (..), TxOutAux,
                                     TxPayload (..), TxProof (..))

-- | Make a pair from 'TxIn'.
txInToPair :: TxIn -> (TxId, Word32)
txInToPair (TxIn h i) = (h, i)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: TxOutAux -> [(StakeholderId, Coin)]
txOutStake (TxOut{..}, mb) = case txOutAddress of
    PubKeyAddress x _ -> [(x, txOutValue)]
    _                 -> mb

-- | Construct 'TxProof' which proves given 'TxPayload'.
mkTxProof :: TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot _txpTxs
    , txpWitnessesHash = hash _txpWitnesses
    }
