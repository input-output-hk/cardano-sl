{-# LANGUAGE TypeOperators #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Block.Core.Main.Misc
       ( mkMainBlock
       , mkMainHeader
       , emptyMainBody
       ) where

import           Universum

import           Control.Monad.Except        (MonadError)
import           Data.Default                (Default (def))
import qualified Data.Text.Buildable         as Buildable
import           Formatting                  (bprint, build, int, stext, (%))
import           Serokell.Util               (Color (Magenta), colorize, listJson)

import           Pos.Binary.Block.Core       ()
import           Pos.Binary.Class            (Bi)
import           Pos.Block.Core.Main.Chain   (Body (..), ConsensusData (..))
import           Pos.Block.Core.Main.Helpers ()
import           Pos.Block.Core.Main.Lens    (mainBlockBlockVersion, mainBlockDifficulty,
                                              mainBlockSlot, mainBlockSoftwareVersion,
                                              mainHeaderBlockVersion,
                                              mainHeaderDifficulty, mainHeaderLeaderKey,
                                              mainHeaderSlot, mainHeaderSoftwareVersion,
                                              mbTxs, mcdDifficulty, mehBlockVersion,
                                              mehSoftwareVersion)
import           Pos.Block.Core.Main.Types   (BlockSignature (..), MainBlock,
                                              MainBlockHeader, MainBlockchain,
                                              MainExtraBodyData (..),
                                              MainExtraHeaderData (..), MainToSign (..))
import           Pos.Block.Core.Union.Types  (BiSsc, BlockHeader, blockHeaderHash)
import           Pos.Core                    (EpochOrSlot (..), GenericBlock (..),
                                              GenericBlockHeader (..),
                                              HasBlockVersion (..), HasDifficulty (..),
                                              HasEpochIndex (..), HasEpochOrSlot (..),
                                              HasHeaderHash (..), HasSoftwareVersion (..),
                                              HeaderHash, IsHeader, IsMainHeader (..),
                                              LocalSlotIndex, SlotId, mkGenericHeader,
                                              recreateGenericBlock, slotIdF)
import           Pos.Core.Configuration      (HasConfiguration)
import           Pos.Crypto                  (ProxySecretKey (..), SecretKey,
                                              SignTag (..), hash, hashHexF, proxySign,
                                              sign, toPublic)
import           Pos.Data.Attributes         (mkAttributes)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Ssc.GodTossing.Type     (SscGodTossing)
import           Pos.Txp.Core                (emptyTxPayload)
import           Pos.Update.Configuration    (HasUpdateConfiguration, curSoftwareVersion,
                                              lastKnownBlockVersion)
import           Pos.Util.Util               (leftToPanic)


instance BiSsc => Buildable MainBlockHeader where
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
        gbhHeaderHash = blockHeaderHash $ Right gbh
        MainConsensusData {..} = _gbhConsensus

instance (HasConfiguration, BiSsc) => Buildable MainBlock where
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
    headerHash = blockHeaderHash . Right

instance Bi BlockHeader =>
         HasHeaderHash MainBlock where
    headerHash = blockHeaderHash . Right . _gbHeader

instance HasDifficulty (ConsensusData $ MainBlockchain ssc) where
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

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

type SanityConstraint
     = ( BiSsc
       , HasDifficulty BlockHeader
       , HasHeaderHash BlockHeader
       , HasConfiguration
       )

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: SanityConstraint
    => Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body (MainBlockchain SscGodTossing)
    -> MainExtraHeaderData
    -> MainBlockHeader
mkMainHeader prevHeader slotId sk pske body extra =
    -- here we know that header creation can't fail, because the only invariant
    -- which we check in 'verifyBBlockHeader' is signature correctness, which
    -- is enforced in this function
    leftToPanic "mkMainHeader: " $
    mkGenericHeader prevHeader body consensus extra
  where
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign (Left psk) =
        BlockPSignatureLight $ proxySign SignMainBlockLight sk psk toSign
    makeSignature toSign (Right (psk,_)) =
        BlockPSignatureHeavy $ proxySign SignMainBlockHeavy sk psk toSign
    signature prevHash proof =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe
               (BlockSignature $ sign SignMainBlock sk toSign)
               (makeSignature toSign)
               pske
    leaderPk = maybe (toPublic sk) (either pskIssuerPk snd) pske
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = leaderPk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

-- | Smart constructor for 'MainBlock'. Uses 'mkMainHeader'. It
-- verifies consistency of given data and may fail.
mkMainBlock
    :: (HasUpdateConfiguration, SanityConstraint, MonadError Text m)
    => Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body (MainBlockchain SscGodTossing)
    -> m MainBlock
mkMainBlock prevHeader slotId sk pske body =
    recreateGenericBlock
        (mkMainHeader prevHeader slotId sk pske body extraH)
        body
        extraB
  where
    extraB :: MainExtraBodyData
    extraB = MainExtraBodyData (mkAttributes ())
    extraH :: MainExtraHeaderData
    extraH =
        MainExtraHeaderData
            lastKnownBlockVersion
            curSoftwareVersion
            (mkAttributes ())
            (hash extraB)

-- | Empty (i. e. no payload) body of main block for given local slot index.
emptyMainBody ::
       forall ssc. SscHelpersClass ssc
    => LocalSlotIndex
    -> Body (MainBlockchain ssc)
emptyMainBody slot =
    MainBody
    { _mbTxPayload = emptyTxPayload
    , _mbSscPayload = sscDefaultPayload @ssc slot
    , _mbDlgPayload = def
    , _mbUpdatePayload = def
    }
