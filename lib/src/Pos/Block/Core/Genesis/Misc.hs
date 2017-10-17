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
import           Pos.Binary.Class             (Bi)
import           Pos.Block.Core.Genesis.Chain (Body (..), ConsensusData (..))
import           Pos.Block.Core.Genesis.Lens  (gcdDifficulty, gcdEpoch)
import           Pos.Block.Core.Genesis.Types (GenesisBlock, GenesisBlockHeader,
                                               GenesisBlockchain,
                                               GenesisExtraBodyData (..),
                                               GenesisExtraHeaderData (..))
import           Pos.Block.Core.Union.Types   (BiSsc, BlockHeader, blockHeaderHash)
import           Pos.Core                     (EpochIndex, EpochOrSlot (..),
                                               GenericBlock (..), GenericBlockHeader (..),
                                               HasConfiguration, HasDifficulty (..),
                                               HasEpochIndex (..), HasEpochOrSlot (..),
                                               HasHeaderHash (..), HeaderHash,
                                               IsGenesisHeader, IsHeader, SlotLeaders,
                                               gbHeader, gbhConsensus, mkGenericHeader,
                                               recreateGenericBlock)
import           Pos.Crypto                   (hashHexF)
import           Pos.Data.Attributes          (mkAttributes)
import           Pos.Ssc.GodTossing.Type      (SscGodTossing)
import           Pos.Util.Util                (leftToPanic)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance BiSsc SscGodTossing => Buildable GenesisBlockHeader where
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

instance BiSsc SscGodTossing => Buildable GenesisBlock where
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

instance HasEpochIndex GenesisBlock where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex GenesisBlockHeader where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot GenesisBlockHeader where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot GenesisBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi GenesisBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance Bi BlockHeader =>
         HasHeaderHash GenesisBlockHeader where
    headerHash = blockHeaderHash . Left

instance Bi BlockHeader =>
         HasHeaderHash GenesisBlock where
    headerHash = blockHeaderHash . Left . _gbHeader

instance HasDifficulty (ConsensusData (GenesisBlockchain SscGodTossing)) where
    difficultyL = gcdDifficulty

instance HasDifficulty GenesisBlockHeader where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty GenesisBlock where
    difficultyL = gbHeader . difficultyL

instance Bi BlockHeader => IsHeader GenesisBlockHeader
instance Bi BlockHeader => IsGenesisHeader GenesisBlockHeader

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

type SanityConstraint ssc
     = ( HasDifficulty BlockHeader
       , HasHeaderHash BlockHeader
       , HasConfiguration
       , ssc ~ SscGodTossing
       )

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: SanityConstraint ssc
    => Maybe BlockHeader
    -> EpochIndex
    -> Body (GenesisBlockchain SscGodTossing)
    -> GenesisBlockHeader
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
    => Maybe BlockHeader
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock
mkGenesisBlock prevHeader epoch leaders =
    leftToPanic "mkGenesisBlock: " $ recreateGenericBlock header body extra
  where
    header = mkGenesisHeader prevHeader epoch body
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()
