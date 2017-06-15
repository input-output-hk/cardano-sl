{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- | Lenses for genesis blockchain types.
--
-- Lenses whose name starts with `genBlock' are from 'GenesisBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'GenesisBlock'. Similar fact is true for `genHeader' prefix.

module Pos.Block.Core.Genesis.Lens
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

import           Control.Lens                 (makeLenses)

import           Pos.Block.Core.Genesis.Chain (Body (..), BodyProof (..),
                                               ConsensusData (..))
import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockHeader,
                                               GenesisBlockchain, GenesisBodyAttributes,
                                               GenesisExtraBodyData (..),
                                               GenesisExtraHeaderData (..),
                                               GenesisHeaderAttributes)
import           Pos.Core                     (ChainDifficulty, EpochIndex (..),
                                               HeaderHash, SlotLeaders, gbBody, gbExtra,
                                               gbHeader, gbPrevBlock, gbhBodyProof,
                                               gbhConsensus, gbhExtra, gbhPrevBlock)

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
genHeaderPrevBlock :: Lens' (GenesisBlockHeader ssc) HeaderHash
genHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'GenesisBlockHeader' to 'GenesisProof'.
genHeaderProof ::
       Lens' (GenesisBlockHeader ssc) (BodyProof $ GenesisBlockchain ssc)
genHeaderProof = gbhBodyProof

-- | Lens from 'GenesisBlockHeader' to 'EpochIndex'.
genHeaderEpoch :: Lens' (GenesisBlockHeader ssc) EpochIndex
genHeaderEpoch = gbhConsensus . gcdEpoch

-- | Lens from 'GenesisBlockHeader' to 'ChainDifficulty'.
genHeaderDifficulty :: Lens' (GenesisBlockHeader ssc) ChainDifficulty
genHeaderDifficulty = gbhConsensus . gcdDifficulty

-- | Lens from 'GenesisBlockHeader' to 'GenesisHeaderAttributes'.
genHeaderAttributes ::
       Lens' (GenesisBlockHeader ssc) GenesisHeaderAttributes
genHeaderAttributes = gbhExtra . gehAttributes

----------------------------------------------------------------------------
-- GenesisBody
----------------------------------------------------------------------------

makeLenses 'GenesisBody

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

-- | Lens from 'GenesisBlock' to 'HeaderHash' of its parent.
genBlockPrevBlock :: Lens' (GenesisBlock ssc) HeaderHash
genBlockPrevBlock = gbPrevBlock

-- | Lens from 'GenesisBlock' to 'GenesisProof'.
genBlockProof :: Lens' (GenesisBlock ssc) (BodyProof $ GenesisBlockchain ssc)
genBlockProof = gbHeader . genHeaderProof

-- | Lens from 'GenesisBlock' to 'EpochIndex'.
genBlockEpoch :: Lens' (GenesisBlock ssc) EpochIndex
genBlockEpoch = gbHeader . genHeaderEpoch

-- | Lens from 'GenesisBlock' to 'ChainDifficulty'.
genBlockDifficulty :: Lens' (GenesisBlock ssc) ChainDifficulty
genBlockDifficulty = gbHeader . genHeaderDifficulty

-- | Lens from 'GenesisBlock' to 'GenesisHeaderAttributes'.
genBlockHeaderAttributes ::
       Lens' (GenesisBlock ssc) GenesisHeaderAttributes
genBlockHeaderAttributes = gbHeader . genHeaderAttributes

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
genBlockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
genBlockLeaders = gbBody . gbLeaders

-- | Lens from 'GenesisBlock' to 'GenesisBodyAttributes'.
genBlockAttributes :: Lens' (GenesisBlock ssc) GenesisBodyAttributes
genBlockAttributes = gbExtra . gebAttributes
