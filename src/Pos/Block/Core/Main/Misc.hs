{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Block.Core.Main.Misc
       (
       ) where

import           Universum

import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, int, stext, (%))
import           Serokell.Util              (Color (Magenta), colorize, listJson)

import           Pos.Block.Core.Main.Chain  (Body (..), ConsensusData (..))
import           Pos.Block.Core.Main.Lens   (mainBlockBlockVersion, mainBlockDifficulty,
                                             mainBlockSlot, mainBlockSlot,
                                             mainBlockSoftwareVersion,
                                             mainHeaderBlockVersion, mainHeaderDifficulty,
                                             mainHeaderLeaderKey, mainHeaderSlot,
                                             mainHeaderSoftwareVersion, mbTxs,
                                             mcdDifficulty, mehBlockVersion,
                                             mehSoftwareVersion)
import           Pos.Block.Core.Main.Types  (MainBlock, MainBlockHeader, MainBlockchain,
                                             MainExtraHeaderData)
import           Pos.Block.Core.Union.Types (BiHeader, BiSsc, blockHeaderHash)
import           Pos.Core                   (EpochOrSlot (..), GenericBlock (..),
                                             GenericBlockHeader (..),
                                             HasBlockVersion (..), HasDifficulty (..),
                                             HasEpochIndex (..), HasEpochOrSlot (..),
                                             HasHeaderHash (..), HasSoftwareVersion (..),
                                             HeaderHash, IsHeader, IsMainHeader (..),
                                             slotIdF)
import           Pos.Crypto                 (hashHexF)

instance BiSsc ssc => Buildable (MainBlockHeader ssc) where
    build gbh@GenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%hashHexF%"\n"%
             "    previous block: "%hashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    leader: "%build%"\n"%
             "    difficulty: "%int%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdLeaderKey
            _mcdDifficulty
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ Right gbh
        MainConsensusData {..} = _gbhConsensus

instance BiSsc ssc => Buildable (MainBlock ssc) where
    build GenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  proxy signing keys ("%int%" items): "%listJson%"\n"%
             build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length txs)
            txs
            (length _mbDlgPayload)
            _mbDlgPayload
            _mbSscPayload
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

instance HasEpochIndex (MainBlock ssc) where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasEpochIndex (MainBlockHeader ssc) where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasEpochOrSlot (MainBlockHeader ssc) where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochOrSlot (MainBlock ssc) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance BiHeader ssc =>
         HasHeaderHash (MainBlockHeader ssc) where
    headerHash = blockHeaderHash . Right

instance BiHeader ssc =>
         HasHeaderHash (MainBlock ssc) where
    headerHash = blockHeaderHash . Right . _gbHeader

instance HasDifficulty (ConsensusData $ MainBlockchain ssc) where
    difficultyL = mcdDifficulty

instance HasDifficulty (MainBlockHeader ssc) where
    difficultyL = mainHeaderDifficulty

instance HasDifficulty (MainBlock ssc) where
    difficultyL = mainBlockDifficulty

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion (MainBlock ssc) where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion (MainBlock ssc) where
    softwareVersionL = mainBlockSoftwareVersion

instance HasBlockVersion (MainBlockHeader ssc) where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion (MainBlockHeader ssc) where
    softwareVersionL = mainHeaderSoftwareVersion

instance BiHeader ssc => IsHeader (MainBlockHeader ssc)

instance BiHeader ssc => IsMainHeader (MainBlockHeader ssc) where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
