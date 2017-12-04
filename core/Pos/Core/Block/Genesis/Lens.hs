{-# LANGUAGE TypeOperators #-}

-- | Lenses for genesis blockchain types.
--
-- Lenses whose name starts with `genBlock' are from 'GenesisBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'GenesisBlock'. Similar fact is true for `genHeader' prefix.

module Pos.Core.Block.Genesis.Lens
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

import           Pos.Core.Block.Blockchain (gbBody, gbExtra, gbHeader, gbPrevBlock, gbhBodyProof,
                                            gbhConsensus, gbhExtra, gbhPrevBlock)
import           Pos.Core.Block.Genesis.Chain (Body (..), BodyProof (..), ConsensusData (..))
import           Pos.Core.Block.Genesis.Types (GenesisBlock, GenesisBlockHeader, GenesisBlockchain,
                                               GenesisBodyAttributes, GenesisExtraBodyData (..),
                                               GenesisExtraHeaderData (..), GenesisHeaderAttributes)
import           Pos.Core.Common (ChainDifficulty, HeaderHash, SlotLeaders)
import           Pos.Core.Slotting.Types (EpochIndex (..))

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData

----------------------------------------------------------------------------
-- ConsensusData
----------------------------------------------------------------------------

makeLenses 'GenesisConsensusData

----------------------------------------------------------------------------
-- GenesisBlockHeader
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlockHeader' to 'HeaderHash' of its parent.
genHeaderPrevBlock :: Lens' GenesisBlockHeader HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'GenesisBlockHeader' to 'GenesisProof'.
genHeaderProof ::
       Lens' GenesisBlockHeader (BodyProof GenesisBlockchain)
genHeaderProof = gbhBodyProof

-- | Lens from 'GenesisBlockHeader' to 'EpochIndex'.
genHeaderEpoch :: Lens' GenesisBlockHeader EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'GenesisBlockHeader' to 'ChainDifficulty'.
genHeaderDifficulty :: Lens' GenesisBlockHeader ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'GenesisBlockHeader' to 'GenesisHeaderAttributes'.
genHeaderAttributes ::
       Lens' GenesisBlockHeader GenesisHeaderAttributes
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
genBlockProof :: Lens' GenesisBlock (BodyProof GenesisBlockchain)
genBlockProof = gbHeader . genHeaderProof

-- | Lens from 'GenesisBlock' to 'EpochIndex'.
genBlockEpoch :: Lens' GenesisBlock EpochIndex
genBlockEpoch = gbHeader . genHeaderEpoch

-- | Lens from 'GenesisBlock' to 'ChainDifficulty'.
genBlockDifficulty :: Lens' GenesisBlock ChainDifficulty
genBlockDifficulty = gbHeader . genHeaderDifficulty

-- | Lens from 'GenesisBlock' to 'GenesisHeaderAttributes'.
genBlockHeaderAttributes ::
       Lens' GenesisBlock GenesisHeaderAttributes
genBlockHeaderAttributes = gbHeader . genHeaderAttributes

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
genBlockLeaders :: Lens' GenesisBlock SlotLeaders
genBlockLeaders = gbBody . gbLeaders

-- | Lens from 'GenesisBlock' to 'GenesisBodyAttributes'.
genBlockAttributes :: Lens' GenesisBlock GenesisBodyAttributes
genBlockAttributes = gbExtra . gebAttributes
