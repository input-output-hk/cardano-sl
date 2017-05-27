{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Block.Core.Genesis.Misc
       ( mkGenesisHeader
       , mkGenesisBlock
       ) where

import           Universum

import qualified Data.Text.Buildable          as Buildable
import           Formatting                   (bprint, build, int, sformat, stext, (%))
import           Serokell.Util                (Color (Magenta), colorize, listJson)

import           Pos.Binary.Block.Core        ()
import           Pos.Block.Core.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Block.Core.Genesis.Lens  (gcdDifficulty, gcdEpoch)
import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockHeader,
                                               GenesisBlockchain,
                                               GenesisExtraBodyData (..),
                                               GenesisExtraHeaderData (..))
import           Pos.Block.Core.Union.Types   (BiHeader, BiSsc, BlockHeader,
                                               blockHeaderHash)
import           Pos.Core                     (EpochIndex, EpochOrSlot (..),
                                               GenericBlock (..), GenericBlockHeader (..),
                                               HasDifficulty (..), HasEpochIndex (..),
                                               HasEpochOrSlot (..), HasHeaderHash (..),
                                               HeaderHash, IsGenesisHeader, IsHeader,
                                               SlotLeaders, gbHeader, gbhConsensus,
                                               mkGenericHeader, recreateGenericBlock)
import           Pos.Crypto                   (hashHexF)
import           Pos.Data.Attributes          (mkAttributes)
import           Pos.Util.Util                (leftToPanic)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance BiSsc ssc => Buildable (GenesisBlockHeader ssc) where
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
        gbhHeaderHash = blockHeaderHash $ Left gbh
        GenesisConsensusData {..} = _gbhConsensus

instance BiSsc ssc => Buildable (GenesisBlock ssc) where
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

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

type SanityConstraint ssc
     = ( HasDifficulty $ BlockHeader ssc
       , HasHeaderHash $ BlockHeader ssc
       )

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: SanityConstraint ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> Body (GenesisBlockchain ssc)
    -> GenesisBlockHeader ssc
mkGenesisHeader prevHeader epoch body =
    -- here we know that genesis header construction can not fail
    leftToPanic "mkGenesisHeader: " $
    mkGenericHeader
        prevHeader
        body
        consensus
        (GenesisExtraHeaderData $ mkAttributes ())
  where
    difficulty = maybe 0 (view difficultyL) prevHeader
    consensus _ _ =
        GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}

-- | Smart constructor for 'GenesisBlock'.
mkGenesisBlock
    :: SanityConstraint ssc
    => Maybe (BlockHeader ssc)
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock ssc
mkGenesisBlock prevHeader epoch leaders =
    leftToPanic "mkGenesisBlock: " $ recreateGenericBlock header body extra
  where
    header = mkGenesisHeader prevHeader epoch body
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()
