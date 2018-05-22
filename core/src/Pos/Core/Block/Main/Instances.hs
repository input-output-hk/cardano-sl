{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Core.Block.Main.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Core.Block ()
import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Block.Main.Chain (Body (..), ConsensusData (..))
import           Pos.Core.Block.Main.Lens (mainBlockBlockVersion, mainBlockDifficulty,
                                           mainBlockSlot, mainBlockSoftwareVersion,
                                           mainHeaderBlockVersion, mainHeaderDifficulty,
                                           mainHeaderLeaderKey, mainHeaderSlot,
                                           mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                                           mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Block.Main.Types (MainBlock, MainBlockHeader, MainBlockchain,
                                            MainExtraHeaderData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..), blockHeaderHash)
import           Pos.Core.Class (HasBlockVersion (..), HasDifficulty (..), HasEpochIndex (..),
                                 HasEpochOrSlot (..), HasHeaderHash (..), HasSoftwareVersion (..),
                                 IsHeader, IsMainHeader (..))
import           Pos.Core.Common (HeaderHash)
import           Pos.Core.Slotting.Types (EpochOrSlot (..), slotIdF)
import           Pos.Crypto (hashHexF)


instance Bi BlockHeader => Buildable MainBlockHeader where
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

instance (Bi BlockHeader) => Buildable MainBlock where
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

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi MainBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance Bi BlockHeader =>
         HasHeaderHash MainBlockHeader where
    headerHash = blockHeaderHash . BlockHeaderMain

instance Bi BlockHeader =>
         HasHeaderHash MainBlock where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

instance HasDifficulty (ConsensusData MainBlockchain) where
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

instance Bi BlockHeader => IsHeader MainBlockHeader

instance Bi BlockHeader => IsMainHeader MainBlockHeader where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
