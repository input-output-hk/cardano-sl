{-# LANGUAGE CPP             #-}
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

-- -- ***TODO*** -- --
-- This comment and macros are copy-pasted and it's bad, but I
-- will either do something with it later or we will just use a
-- solution.
-- -- ***TODO*** -- --

-- !!! Create issue about this on lens github or give link on existing issue !!!
-- 'makeLensesData' doesn't work with types with parameters. I don't
-- know how to design a 'makeLensesData' which would work with them (in fact,
-- I don't even know how an invocation of 'makeLensesData' would look like)
--
-- UPDATE: the issue is https://github.com/ekmett/lens/issues/733

#define MAKE_LENS(l, field) l f s = (\y -> s {field = y}) <$> f (field s)

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''GenesisExtraHeaderData
makeLenses ''GenesisExtraBodyData

----------------------------------------------------------------------------
-- ConsensusData
----------------------------------------------------------------------------

-- makeLensesData ''ConsensusData ''(GenesisBlockchain ssc)

-- | Lens for 'EpochIndex' of 'GenesisBlockchain' in 'ConsensusData'.
gcdEpoch :: Lens' (ConsensusData (GenesisBlockchain ssc)) EpochIndex
MAKE_LENS(gcdEpoch, _gcdEpoch)

-- | Lens for 'ChainDifficulty' of 'GenesisBlockchain' in 'ConsensusData'.
gcdDifficulty :: Lens' (ConsensusData (GenesisBlockchain ssc)) ChainDifficulty
MAKE_LENS(gcdDifficulty, _gcdDifficulty)

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

-- makeLensesData ''Body ''(GenesisBlockchain ssc)

-- | Lens for 'SlotLeaders' in 'Body' of 'GenesisBlockchain'.
gbLeaders :: Lens' (Body (GenesisBlockchain ssc)) SlotLeaders
MAKE_LENS(gbLeaders, _gbLeaders)

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
