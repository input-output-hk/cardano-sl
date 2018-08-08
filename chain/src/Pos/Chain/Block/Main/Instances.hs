{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Chain.Block.Main.Instances
       (
       ) where

import           Universum

import           Formatting (bprint, build, int, stext, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Chain.Block.Blockchain (GenericBlock (..))
import           Pos.Chain.Block.Main.Types (MainBody (..),
                     MainExtraHeaderData (..))
import           Pos.Chain.Block.Union.Types (BlockHeader (..),
                     HasHeaderHash (..), IsHeader, IsMainHeader (..),
                     MainBlock, MainBlockHeader, MainConsensusData (..),
                     blockHeaderHash, mainBlockBlockVersion, mainBlockSlot,
                     mainBlockSoftwareVersion, mainHeaderBlockVersion,
                     mainHeaderLeaderKey, mainHeaderSlot,
                     mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                     mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Common (HasDifficulty (..))
import           Pos.Core.Slotting (HasEpochIndex (..), HasEpochOrSlot (..))
import           Pos.Core.Update (HasBlockVersion (..), HasSoftwareVersion (..))

instance NFData MainBlock

instance Buildable MainBlock where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  "%build%"\n"%
             "  "%build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length txs)
            txs
            _mbDlgPayload
            _mbSscPayload
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

instance HasEpochIndex MainBlock where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasEpochOrSlot MainBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash MainBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderMain

instance HasHeaderHash MainBlock where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

instance HasDifficulty MainConsensusData where
    difficultyL = mcdDifficulty

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion MainBlock where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion MainBlock where
    softwareVersionL = mainBlockSoftwareVersion

instance HasBlockVersion MainBlockHeader where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion MainBlockHeader where
    softwareVersionL = mainHeaderSoftwareVersion

instance IsHeader MainBlockHeader

instance IsMainHeader MainBlockHeader where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
