-- | Constants from GenesisSpec

module Pos.Core.Genesis.Constants
       ( accountGenesisIndex
       , wAddressGenesisIndex
       ) where

import           Universum

import           Pos.Crypto.HD              (firstHardened)

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened
