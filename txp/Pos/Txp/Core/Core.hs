-- | Helper for core types.

module Pos.Txp.Core.Core
       ( addrBelongsTo
       , addrBelongsToSet
       , emptyTxPayload
       , flattenTxPayload
       , mkTxProof
       , txOutStake
       ) where

import           Universum

import qualified Data.HashSet        as HS
import           Data.List           (zipWith3)

import           Pos.Binary.Core     ()
import           Pos.Binary.Crypto   ()
import           Pos.Binary.Txp.Core ()
import           Pos.Core            (Address (..))
import           Pos.Crypto          (hash)
import           Pos.Merkle          (mtRoot)
import           Pos.Txp.Core.Types  (TxAux (..), TxOut (..), TxOutAux (..),
                                      TxOutDistribution, TxPayload (..), TxProof (..),
                                      mkTxPayload)
import           Pos.Util.Util       (leftToPanic)

-- | A predicate for `TxOutAux` which checks whether given address
-- belongs to it.
addrBelongsTo :: TxOutAux -> Address -> Bool
addrBelongsTo TxOutAux {..} = (== txOutAddress toaOut)

-- | Extended version of `addBelongsTo`, allows to compare against several
-- addresses.
addrBelongsToSet :: TxOutAux -> HashSet Address -> Bool
TxOutAux {..} `addrBelongsToSet` addrs = txOutAddress toaOut `HS.member` addrs

-- | Use this function if you need to know how a 'TxOut' distributes stake
-- (e.g. for the purpose of running follow-the-satoshi).
--
-- [CSL-1489] TODO: we should get rid of 'TxOutAux' and take
-- distribution from 'Address'.
txOutStake :: TxOutAux -> TxOutDistribution
txOutStake TxOutAux {..} = toaDistr

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

emptyTxPayload :: TxPayload
emptyTxPayload = leftToPanic @Text "emptyTxPayload failed" $ mkTxPayload []
