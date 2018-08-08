{-# LANGUAGE TypeOperators #-}

-- | Lenses for genesis blockchain types.
--
-- Lenses whose name starts with `genBlock' are from 'GenesisBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'GenesisBlock'. Similar fact is true for `genHeader' prefix.

module Pos.Chain.Block.Genesis.Lens
       (
         -- * Extra types
         gehAttributes
       , gebAttributes

         -- * GenesisConsensusData
       , gcdEpoch
       , gcdDifficulty

         -- * GenesisBlockHeader
       , genHeaderPrevBlock
       , genHeaderProof
       , genHeaderEpoch
       , genHeaderDifficulty
       , genHeaderAttributes

         -- * GenesisBody
       , gbLeaders

         -- * GenesisBlock
       , genBlockPrevBlock
       , genBlockProof
       , genBlockEpoch
       , genBlockDifficulty
       , genBlockHeaderAttributes
       , genBlockLeaders
       , genBlockAttributes
       ) where

import           Universum

import           Control.Lens (makeLenses)

import           Pos.Chain.Block.Blockchain (gbBody, gbExtra, gbHeader,
                     gbPrevBlock, gbhBodyProof, gbhConsensus, gbhExtra,
                     gbhPrevBlock)
import           Pos.Chain.Block.Genesis.Types (GenesisBody (..),
                     GenesisBodyAttributes, GenesisExtraBodyData (..),
                     GenesisExtraHeaderData (..), GenesisHeaderAttributes,
                     GenesisProof (..), gcdDifficulty, gcdEpoch)
import           Pos.Chain.Block.Union.Types (GenesisBlock, GenesisBlockHeader,
                     HeaderHash)
import           Pos.Core.Common (ChainDifficulty, SlotLeaders)
import           Pos.Core.Slotting (EpochIndex (..))

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData

----------------------------------------------------------------------------
-- ConsensusData
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- GenesisBlockHeader
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlockHeader' to 'HeaderHash' of its parent.
genHeaderPrevBlock :: Lens' GenesisBlockHeader HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'GenesisBlockHeader' to 'GenesisProof'.
genHeaderProof :: Lens' GenesisBlockHeader GenesisProof
genHeaderProof = gbhBodyProof

-- | Lens from 'GenesisBlockHeader' to 'EpochIndex'.
genHeaderEpoch :: Lens' GenesisBlockHeader EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'GenesisBlockHeader' to 'ChainDifficulty'.
genHeaderDifficulty :: Lens' GenesisBlockHeader ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'GenesisBlockHeader' to 'GenesisHeaderAttributes'.
genHeaderAttributes :: Lens' GenesisBlockHeader GenesisHeaderAttributes
genHeaderAttributes = gbhExtra . gehAttributes

----------------------------------------------------------------------------
-- GenesisBody
----------------------------------------------------------------------------

makeLenses 'GenesisBody

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlock' to 'HeaderHash' of its parent.
genBlockPrevBlock :: Lens' GenesisBlock HeaderHash
genBlockPrevBlock = gbPrevBlock

-- | Lens from 'GenesisBlock' to 'GenesisProof'.
genBlockProof :: Lens' GenesisBlock GenesisProof
genBlockProof = gbHeader . genHeaderProof

-- | Lens from 'GenesisBlock' to 'EpochIndex'.
genBlockEpoch :: Lens' GenesisBlock EpochIndex
genBlockEpoch = gbHeader . genHeaderEpoch

-- | Lens from 'GenesisBlock' to 'ChainDifficulty'.
genBlockDifficulty :: Lens' GenesisBlock ChainDifficulty
genBlockDifficulty = gbHeader . genHeaderDifficulty

-- | Lens from 'GenesisBlock' to 'GenesisHeaderAttributes'.
genBlockHeaderAttributes :: Lens' GenesisBlock GenesisHeaderAttributes
genBlockHeaderAttributes = gbHeader . genHeaderAttributes

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
genBlockLeaders :: Lens' GenesisBlock SlotLeaders
genBlockLeaders = gbBody . gbLeaders

-- | Lens from 'GenesisBlock' to 'GenesisBodyAttributes'.
genBlockAttributes :: Lens' GenesisBlock GenesisBodyAttributes
genBlockAttributes = gbExtra . gebAttributes
