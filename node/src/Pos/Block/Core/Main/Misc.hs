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
import           Pos.Block.Core.Union.Types  (BiHeader, BiSsc, BlockHeader,
                                              blockHeaderHash)
import           Pos.Core                    (EpochOrSlot (..), GenericBlock (..),
                                              GenericBlockHeader (..),
                                              HasBlockVersion (..),
                                              HasDifficulty (..), HasEpochIndex (..),
                                              HasEpochOrSlot (..), HasHeaderHash (..),
                                              HasSoftwareVersion (..), HeaderHash,
                                              IsHeader, IsMainHeader (..), LocalSlotIndex,
                                              SlotId, mkGenericHeader,
                                              recreateGenericBlock, slotIdF)
import           Pos.Core.Configuration      (HasConfiguration)
import           Pos.Crypto                  (ProxySecretKey (..), SecretKey,
                                              SignTag (..), hash, hashHexF, proxySign,
                                              sign, toPublic)
import           Pos.Data.Attributes         (mkAttributes)
import           Pos.Delegation.Types        (ProxySKBlockInfo)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass (..))
import           Pos.Txp.Core                (emptyTxPayload)
import           Pos.Update.Configuration    (HasUpdateConfiguration,
                                              lastKnownBlockVersion, curSoftwareVersion)
import           Pos.Util.Util               (leftToPanic)

instance BiSsc ssc => Buildable (MainBlockHeader ssc) where
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

instance (HasConfiguration, BiSsc ssc) => Buildable (MainBlock ssc) where
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

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

type SanityConstraint ssc
     = ( BiSsc ssc
       , SscHelpersClass ssc
       , HasDifficulty $ BlockHeader ssc
       , HasHeaderHash $ BlockHeader ssc
       , HasConfiguration
       )

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: (SanityConstraint ssc)
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body (MainBlockchain ssc)
    -> MainExtraHeaderData
    -> MainBlockHeader ssc
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
    :: (HasUpdateConfiguration, SanityConstraint ssc, MonadError Text m)
    => Maybe (BlockHeader ssc)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body (MainBlockchain ssc)
    -> m (MainBlock ssc)
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
