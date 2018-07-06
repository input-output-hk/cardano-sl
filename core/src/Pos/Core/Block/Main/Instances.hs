{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Core.Block.Main.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Core.Block.Blockchain (GenericBlock (..),
                     GenericBlockHeader (..))
import           Pos.Core.Block.Main.Lens (mainBlockBlockVersion,
                     mainBlockDifficulty, mainBlockSlot,
                     mainBlockSoftwareVersion, mainHeaderBlockVersion,
                     mainHeaderDifficulty, mainHeaderLeaderKey, mainHeaderSlot,
                     mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                     mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Block.Main.Types (MainBody (..),
                     MainExtraHeaderData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..),
                     HasHeaderHash (..), HeaderHash, IsHeader,
                     IsMainHeader (..), MainBlock, MainBlockHeader,
                     MainConsensusData (..), blockHeaderHash)
import           Pos.Core.Common (HasDifficulty (..))
import           Pos.Core.Slotting (EpochOrSlot (..), HasEpochIndex (..),
                     HasEpochOrSlot (..), slotIdF)
import           Pos.Core.Update (HasBlockVersion (..), HasSoftwareVersion (..))
import           Pos.Crypto (hashHexF)

instance NFData MainBlock

instance Buildable MainBlockHeader where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    difficulty: "%int%"\n"%
             "    leader: "%build%"\n"%
             "    signature: "%build%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdDifficulty
            _mcdLeaderKey
            _mcdSignature
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderMain gbh
        MainConsensusData {..} = _gbhConsensus

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

instance HasEpochIndex MainBlockHeader where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasEpochOrSlot MainBlockHeader where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochOrSlot MainBlock where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash MainBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderMain

instance HasHeaderHash MainBlock where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

instance HasDifficulty MainConsensusData where
    difficultyL = mcdDifficulty

instance HasDifficulty MainBlockHeader where
    difficultyL = mainHeaderDifficulty

instance HasDifficulty MainBlock where
    difficultyL = mainBlockDifficulty

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
