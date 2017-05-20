{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Lenses for genesis blockchain types.

module Pos.Block.Core.Genesis.Lens
       ( gcdEpoch
       , gcdDifficulty
       , gbLeaders
       , blockLeaders
       ) where

import           Universum

import           Pos.Block.Core.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockchain)
import           Pos.Core                     (ChainDifficulty, EpochIndex (..),
                                               SlotLeaders, gbBody)

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

-- makeLensesData ''ConsensusData ''(GenesisBlockchain ssc)

-- | Lens for 'EpochIndex' of 'GenesisBlockchain' in 'ConsensusData'.
gcdEpoch :: Lens' (ConsensusData (GenesisBlockchain ssc)) EpochIndex
MAKE_LENS(gcdEpoch, _gcdEpoch)

-- | Lens for 'ChainDifficulty' of 'GenesisBlockchain' in 'ConsensusData'.
gcdDifficulty :: Lens' (ConsensusData (GenesisBlockchain ssc)) ChainDifficulty
MAKE_LENS(gcdDifficulty, _gcdDifficulty)

-- makeLensesData ''Body ''(GenesisBlockchain ssc)

-- | Lens for 'SlotLeaders' in 'Body' of 'GenesisBlockchain'.
gbLeaders :: Lens' (Body (GenesisBlockchain ssc)) SlotLeaders
MAKE_LENS(gbLeaders, _gbLeaders)

-- | Lens from 'GenesisBlock' to 'SlotLeaders'.
blockLeaders :: Lens' (GenesisBlock ssc) SlotLeaders
blockLeaders = gbBody . gbLeaders
