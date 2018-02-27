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

import           Pos.Block.BHelpers ()
import           Pos.Core (BlockVersion, EpochIndex, HasDifficulty (..), LocalSlotIndex, SlotId,
                           SlotLeaders, SoftwareVersion, GenesisHash (..), HasProtocolConstants)
import           Pos.Core.Block (BlockHeader, BlockSignature (..), GenesisBlock, GenesisBlockHeader,
                                 GenesisBlockchain, GenesisExtraBodyData (..),
                                 GenesisExtraHeaderData (..), MainBlock, MainBlockHeader,
                                 MainBlockchain, MainExtraBodyData (..), MainExtraHeaderData (..),
                                 MainToSign (..), mkGenericHeader, GenericBlock (..))
import           Pos.Core.Block.Genesis (Body (..), ConsensusData (..))
import           Pos.Core.Block.Main (Body (..), ConsensusData (..))
import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (..), hash, proxySign, sign, toPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Delegation.Types (ProxySKBlockInfo)
import           Pos.Ssc.Base (defaultSscPayload)
import           Pos.Txp.Base (emptyTxPayload)

----------------------------------------------------------------------------
-- Main smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body MainBlockchain
    -> MainExtraHeaderData
    -> MainBlockHeader
mkMainHeader pm prevHeader slotId sk pske body extra =
    mkGenericHeader pm prevHeader body consensus extra
  where
    difficulty = either (const 0) (succ . view difficultyL) prevHeader
    makeSignature toSign (psk,_) =
        BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
    signature prevHash proof =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe
               (BlockSignature $ sign pm SignMainBlock sk toSign)
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
mkMainBlock
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> Either GenesisHash BlockHeader
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> Body MainBlockchain
    -> MainBlock
mkMainBlock pm bv sv prevHeader slotId sk pske body =
    UnsafeGenericBlock
        (mkMainHeader pm prevHeader slotId sk pske body extraH)
        body
        extraB
  where
    extraB :: MainExtraBodyData
    extraB = MainExtraBodyData (mkAttributes ())
    extraH :: MainExtraHeaderData
    extraH =
        MainExtraHeaderData
            bv
            sv
            (mkAttributes ())
            (hash extraB)

-- | Empty (i. e. no payload) body of main block for given local slot index.
emptyMainBody
    :: HasProtocolConstants
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
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> EpochIndex
    -> Body GenesisBlockchain
    -> GenesisBlockHeader
mkGenesisHeader pm prevHeader epoch body =
    -- here we know that genesis header construction can not fail
    mkGenericHeader
        pm
        prevHeader
        body
        consensus
        (GenesisExtraHeaderData $ mkAttributes ())
  where
    difficulty = either (const 0) (view difficultyL) prevHeader
    consensus _ _ =
        GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty}

-- | Smart constructor for 'GenesisBlock'.
mkGenesisBlock
    :: ProtocolMagic
    -> Either GenesisHash BlockHeader
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock
mkGenesisBlock pm prevHeader epoch leaders =
    UnsafeGenericBlock header body extra
  where
    header = mkGenesisHeader pm prevHeader epoch body
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()

-- | Creates the very first genesis block.
genesisBlock0 :: ProtocolMagic -> GenesisHash -> SlotLeaders -> GenesisBlock
genesisBlock0 pm genesisHash leaders = mkGenesisBlock pm (Left genesisHash) 0 leaders
