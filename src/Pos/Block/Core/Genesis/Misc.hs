-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Block.Core.Genesis.Misc
       (
       ) where

import           Universum

import qualified Data.Text.Buildable          as Buildable
import           Formatting                   (bprint, build, int, sformat, stext, (%))
import           Serokell.Util                (Color (Magenta), colorize, listJson)

import           Pos.Block.Core.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Block.Core.Genesis.Lens  (gcdDifficulty, gcdEpoch)
import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockHeader,
                                               GenesisBlockchain)
import           Pos.Block.Core.Union.Types   (BiHeader, BiSsc, blockHeaderHash)
import           Pos.Core                     (EpochOrSlot (..), GenericBlock (..),
                                               GenericBlockHeader (..),
                                               HasDifficulty (..), HasEpochIndex (..),
                                               HasEpochOrSlot (..), HasHeaderHash (..),
                                               HeaderHash, IsGenesisHeader, IsHeader,
                                               gbHeader, gbhConsensus)
import           Pos.Crypto                   (hashHexF)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance BiSsc ssc => Buildable (GenesisBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
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
        gbhHeaderHash = blockHeaderHash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

instance BiSsc ssc => Buildable (GenesisBlock ssc) where
    build GenericBlock {..} =
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
            ("  leaders: "%listJson%"\n") _gbLeaders

----------------------------------------------------------------------------
-- Pos.Core.Class
----------------------------------------------------------------------------

instance HasEpochIndex (GenesisBlock ssc) where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex (GenesisBlockHeader ssc) where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot (GenesisBlockHeader ssc) where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot (GenesisBlock ssc) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance BiHeader ssc =>
         HasHeaderHash (GenesisBlockHeader ssc) where
    headerHash = blockHeaderHash . Left

instance BiHeader ssc =>
         HasHeaderHash (GenesisBlock ssc) where
    headerHash = blockHeaderHash . Left . _gbHeader

instance HasDifficulty (ConsensusData (GenesisBlockchain ssc)) where
    difficultyL = gcdDifficulty

instance HasDifficulty (GenesisBlockHeader ssc) where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty (GenesisBlock ssc) where
    difficultyL = gbHeader . difficultyL

instance BiHeader ssc => IsHeader (GenesisBlockHeader ssc)
instance BiHeader ssc => IsGenesisHeader (GenesisBlockHeader ssc)
