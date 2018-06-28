module Pos.Core.Txp.TxProof
       ( TxProof (..)
       , mkTxProof
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Crypto (Hash, hash)
import           Pos.Merkle (MerkleRoot, mkMerkleTree, mtRoot)

import           Pos.Core.Txp.Tx
import           Pos.Core.Txp.TxPayload
import           Pos.Core.Txp.TxWitness

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
mkTxProof :: Bi TxInWitness => TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot (mkMerkleTree _txpTxs)
    , txpWitnessesHash = hash _txpWitnesses
    }
