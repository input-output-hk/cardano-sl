{-# LANGUAGE RecordWildCards #-}

module Pos.Chain.Txp.TxProof
       ( TxProof (..)
       , mkTxProof
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Fmt (genericF)
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core.Merkle (MerkleRoot, mkMerkleTree, mtRoot)
import           Pos.Crypto (Hash, hash)

import           Pos.Chain.Txp.Tx
import           Pos.Chain.Txp.TxPayload
import           Pos.Chain.Txp.TxWitness

data TxProof = TxProof
    { txpNumber        :: !Word32
    , txpRoot          :: !(MerkleRoot Tx)
    , txpWitnessesHash :: !(Hash [TxWitness])
    } deriving (Show, Eq, Generic)

instance Buildable TxProof where
    build = genericF

instance Bi TxProof where
    encode proof =  encodeListLen 3
                 <> encode (txpNumber proof)
                 <> encode (txpRoot proof)
                 <> encode (txpWitnessesHash proof)
    decode = do
        enforceSize "TxProof" 3
        TxProof <$> decode <*>
                    decode <*>
                    decode

instance NFData TxProof

-- | Construct 'TxProof' which proves given 'TxPayload'.
-- This will construct a merkle tree, which can be very expensive. Use with
-- care. Bi constraints arise because we need to hash these things.
mkTxProof :: TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot (mkMerkleTree _txpTxs)
    , txpWitnessesHash = hash _txpWitnesses
    }

deriveSafeCopySimple 0 'base ''TxProof
