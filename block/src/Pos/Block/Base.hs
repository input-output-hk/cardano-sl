-- | Block constructors and basic functions.

module Pos.Block.Base
       ( mkMainBlock
       , mkMainHeader
       , emptyMainBody

       , mkGenesisHeader
       , mkGenesisBlock
       , genesisBlock0
       ) where

import           Universum

import           Data.Default (Default (def))

import           Pos.Core (EpochIndex, HasConfiguration, HasDifficulty (..), LocalSlotIndex, SlotId,
                           SlotLeaders)
import           Pos.Core.Block (BlockHeader, BlockSignature (..), GenericBlock (..), GenesisBlock,
                                 GenesisBlockHeader, GenesisBlockchain, GenesisExtraBodyData (..),
                                 GenesisExtraHeaderData (..), MainBlock, MainBlockHeader,
                                 MainBlockchain, MainExtraBodyData (..), MainExtraHeaderData (..),
                                 MainToSign (..), mkGenericHeader)
import           Pos.Core.Block.Genesis (Body (..), ConsensusData (..))
import           Pos.Core.Block.Main (Body (..), ConsensusData (..))
import           Pos.Crypto (SecretKey, SignTag (..), hash, proxySign, sign, toPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Delegation.Types (ProxySKBlockInfo)
import           Pos.Lrc.Genesis (genesisLeaders)
import           Pos.Ssc.Base (defaultSscPayload)
import           Pos.Txp.Base (emptyTxPayload)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion,
                                           lastKnownBlockVersion)

----------------------------------------------------------------------------
-- Main smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: HasConfiguration
    => Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body MainBlockchain
    -> MainExtraHeaderData
    -> MainBlockHeader
mkMainHeader prevHeader slotId sk pske body extra =
    mkGenericHeader prevHeader body consensus extra
  where
    difficulty = maybe 0 (succ . view difficultyL) prevHeader
    makeSignature toSign (psk,_) =
        BlockPSignatureHeavy $ proxySign SignMainBlockHeavy sk psk toSign
    signature prevHash proof =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe
               (BlockSignature $ sign SignMainBlock sk toSign)
               (makeSignature toSign)
               pske
    leaderPk = maybe (toPublic sk) snd pske
    consensus prevHash proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = leaderPk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature prevHash proof
        }

-- | Smart constructor for 'MainBlock'.
--
-- FIXME TBD do we need to verify here? This is not used on untrusted data,
-- so why bother?
mkMainBlock
    :: (HasUpdateConfiguration, HasConfiguration)
    => Maybe BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body MainBlockchain
    -> MainBlock
mkMainBlock prevHeader slotId sk pske body =
    UncheckedGenericBlock
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
emptyMainBody
    :: HasConfiguration
    => LocalSlotIndex
    -> Body MainBlockchain
emptyMainBody slot =
    MainBody
    { _mbTxPayload = emptyTxPayload
    , _mbSscPayload = defaultSscPayload slot
    , _mbDlgPayload = def
    , _mbUpdatePayload = def
    }

----------------------------------------------------------------------------
-- Genesis smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: HasConfiguration
    => Maybe BlockHeader
    -> EpochIndex
    -> Body GenesisBlockchain
    -> GenesisBlockHeader
mkGenesisHeader prevHeader epoch body =
    -- here we know that genesis header construction can not fail
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
    :: HasConfiguration
    => Maybe BlockHeader
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock
mkGenesisBlock prevHeader epoch leaders =
    UncheckedGenericBlock header body extra
  where
    header = mkGenesisHeader prevHeader epoch body
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()

-- | Creates the very first genesis block.
genesisBlock0 :: HasConfiguration => GenesisBlock
genesisBlock0 = mkGenesisBlock Nothing 0 genesisLeaders
