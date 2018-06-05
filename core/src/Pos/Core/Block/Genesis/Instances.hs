{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Core.Block.Genesis.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, sformat, stext, (%))
import           Serokell.Util (Color (Magenta), colorize)

import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..), gbHeader,
                                            gbhConsensus)
import           Pos.Core.Block.Genesis.Lens (gcdDifficulty, gcdEpoch)
import           Pos.Core.Block.Genesis.Types (GenesisBody (..), GenesisConsensusData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..), GenesisBlock, GenesisBlockHeader,
                                             HasHeaderHash (..), HeaderHash, IsGenesisHeader,
                                             IsHeader, blockHeaderHash)
import           Pos.Core.Common (HasDifficulty (..), slotLeadersF)
import           Pos.Core.Slotting (EpochOrSlot (..), HasEpochIndex (..), HasEpochOrSlot (..))
import           Pos.Crypto (hashHexF)

instance NFData GenesisBlock

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Buildable GenesisBlockHeader where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            gbhHeaderHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderGenesis gbh
        GenesisConsensusData {..} = _gbhConsensus

instance Buildable GenesisBlock where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             stext
            )
            (colorize Magenta "GenesisBlock")
            _gbHeader
            formatLeaders
      where
        GenesisBody {..} = _gbBody
        formatIfNotNull formatter l = if null l then mempty else sformat formatter l
        formatLeaders = formatIfNotNull
            ("  leaders: "%slotLeadersF%"\n") (toList _gbLeaders)

instance HasEpochIndex GenesisBlock where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex GenesisBlockHeader where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot GenesisBlockHeader where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot GenesisBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash GenesisBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderGenesis

instance HasHeaderHash GenesisBlock where
    headerHash = blockHeaderHash . BlockHeaderGenesis . _gbHeader

instance HasDifficulty GenesisConsensusData where
    difficultyL = gcdDifficulty

instance HasDifficulty GenesisBlockHeader where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty GenesisBlock where
    difficultyL = gbHeader . difficultyL

instance IsHeader GenesisBlockHeader
instance IsGenesisHeader GenesisBlockHeader
