-- | Helper for core types.

module Pos.Txp.Core.Core
       ( addrBelongsTo
       , addrBelongsToSet
       , mkTxProof
       , txInToPair
       , txOutStake
       , flattenTxPayload
       ) where

import           Universum

import qualified Data.HashSet        as HS
import           Data.List           (zipWith3)

import           Pos.Binary.Core     ()
import           Pos.Binary.Crypto   ()
import           Pos.Binary.Txp.Core ()
import           Pos.Core.Address    (AddressIgnoringAttributes (..))
import           Pos.Core.Types      (Address (..))
import           Pos.Crypto          (hash)
import           Pos.Merkle          (mtRoot)
import           Pos.Txp.Core.Types  (TxAux (..), TxId, TxIn (..), TxOut (..),
                                      TxOutAux (..), TxOutDistribution, TxPayload (..),
                                      TxProof (..))

-- | A predicate for `TxOutAux` which checks whether given address
-- belongs to it.
addrBelongsTo :: TxOutAux -> Address -> Bool
addrBelongsTo TxOutAux {..} = ((==) `on` AddressIA) (txOutAddress toaOut)

-- | Extended version of `addBelongsTo`, allows to compare against several
-- addresses.
addrBelongsToSet :: TxOutAux -> HashSet AddressIgnoringAttributes -> Bool
TxOutAux {..} `addrBelongsToSet` addrs =
    AddressIA (txOutAddress toaOut) `HS.member` addrs

-- | Make a pair from 'TxIn'.
txInToPair :: TxIn -> (TxId, Word32)
txInToPair (TxIn h i) = (h, i)

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
txOutStake :: TxOutAux -> TxOutDistribution
txOutStake TxOutAux {..} = case txOutAddress toaOut of
    PubKeyAddress x _ -> [(x, txOutValue toaOut)]
    _                 -> toaDistr

-- | Construct 'TxProof' which proves given 'TxPayload'.
mkTxProof :: TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot _txpTxs
    , txpWitnessesHash = hash _txpWitnesses
    , txpDistributionsHash = hash _txpDistributions
    }

-- | Convert 'TxPayload' into a flat list of `TxAux`s.
flattenTxPayload :: TxPayload -> [TxAux]
flattenTxPayload UnsafeTxPayload {..} =
    zipWith3 TxAux (toList _txpTxs) _txpWitnesses _txpDistributions
