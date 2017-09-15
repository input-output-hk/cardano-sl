-- | Constants from GenesisSpec

module Pos.Core.Genesis.Constants
       ( genesisBlockVersionData
       , genesisMpcThd
       , genesisHeavyDelThd
       , genesisUpdateVoteThd
       , genesisSlotDuration
       , genesisMaxBlockSize

       -- Protocol constants
       , vssMinTTL
       , vssMaxTTL

       -- Other constants
       , accountGenesisIndex
       , wAddressGenesisIndex
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Genesis.Parser    (genesisSpec)
import           Pos.Core.Genesis.Types     (gsBlockVersionData)
import           Pos.Core.Types             (BlockVersionData (..), CoinPortion)
import           Pos.Crypto                 (firstHardened)

-- | Genesis 'BlockVersionData'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData = gsBlockVersionData genesisSpec

genesisMpcThd :: CoinPortion
genesisMpcThd = bvdMpcThd genesisBlockVersionData

genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = bvdHeavyDelThd genesisBlockVersionData

genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = bvdUpdateVoteThd genesisBlockVersionData

genesisSlotDuration :: Millisecond
genesisSlotDuration = bvdSlotDuration genesisBlockVersionData

genesisMaxBlockSize :: Byte
genesisMaxBlockSize = bvdMaxBlockSize genesisBlockVersionData

-- Protocol constants

-- TODO will read from genSpec
-- | VSS certificates max timeout to live (number of epochs)
vssMaxTTL :: Integral i => i
vssMaxTTL = 2

-- TODO will read from genSpec
-- | VSS certificates min timeout to live (number of epochs)
vssMinTTL :: Integral i => i
vssMinTTL = 6

-- Other constants

-- | First index in derivation path for HD account, which is put to genesis utxo
accountGenesisIndex :: Word32
accountGenesisIndex = firstHardened

-- | Second index in derivation path for HD account, which is put to genesis
-- utxo
wAddressGenesisIndex :: Word32
wAddressGenesisIndex = firstHardened
