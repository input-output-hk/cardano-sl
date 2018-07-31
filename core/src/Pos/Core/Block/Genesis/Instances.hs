{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Core.Block.Genesis.Instances
       (
       ) where

import           Universum

import           Formatting (bprint, build, sformat, stext, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (Color (Magenta), colorize)

import           Pos.Core.Block.Blockchain (GenericBlock (..), gbHeader,
                     gbhConsensus)
import           Pos.Core.Block.Genesis.Lens (gcdEpoch)
import           Pos.Core.Block.Genesis.Types (GenesisBody (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..), GenesisBlock,
                     GenesisBlockHeader, HasHeaderHash (..), IsGenesisHeader,
                     IsHeader, blockHeaderHash)
import           Pos.Core.Common (slotLeadersF)
import           Pos.Core.Slotting (HasEpochIndex (..), HasEpochOrSlot (..))

instance NFData GenesisBlock

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

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

instance HasEpochOrSlot GenesisBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash GenesisBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderGenesis

instance HasHeaderHash GenesisBlock where
    headerHash = blockHeaderHash . BlockHeaderGenesis . _gbHeader

instance IsHeader GenesisBlockHeader
instance IsGenesisHeader GenesisBlockHeader
