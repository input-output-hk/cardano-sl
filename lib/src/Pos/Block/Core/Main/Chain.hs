{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Definitions of the main blockchain ('Blockchain' class and related).

module Pos.Block.Core.Main.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import           Pos.Binary.Class           (Bi)
import           Pos.Binary.Core            ()
import           Pos.Binary.Delegation      ()
import           Pos.Binary.Txp             ()
import           Pos.Binary.Update          ()
import           Pos.Block.Core.Main.Types  (MainBlock, MainBlockchain, MainExtraBodyData,
                                             MainExtraHeaderData, MainToSign (..))
import           Pos.Block.Core.Union.Types (Block, BlockHeader, BlockSignature (..))
import           Pos.Core                   (Blockchain (..), ChainDifficulty,
                                             GenericBlockHeader (..), HasConfiguration,
                                             IsMainHeader (..), SlotId (..))
import           Pos.Crypto                 (Hash, PublicKey, hash)
import           Pos.Delegation.Types       (DlgPayload)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Txp.Core               (TxPayload, TxProof, mkTxProof)
import           Pos.Update.Core.Types      (UpdatePayload, UpdateProof, mkUpdateProof)

instance ( HasConfiguration
         , Bi BlockHeader
         , Ssc
         , Bi (BodyProof MainBlockchain)
         , IsMainHeader (GenericBlockHeader MainBlockchain)) =>
         Blockchain MainBlockchain where

    -- | Proof of everything contained in the payload.
    data BodyProof MainBlockchain = MainProof
        { mpTxProof       :: !TxProof
        , mpMpcProof      :: !SscProof
        , mpProxySKsProof :: !(Hash DlgPayload)
        , mpUpdateProof   :: !UpdateProof
        } deriving (Eq, Show, Generic)

    data ConsensusData MainBlockchain = MainConsensusData
        { -- | Id of the slot for which this block was generated.
          _mcdSlot       :: !SlotId
        , -- | Public key of the slot leader. It's essential to have it here,
          -- because FTS gives us only hash of public key (aka 'StakeholderId').
          _mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
          _mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
          _mcdSignature  :: !BlockSignature
        } deriving (Generic, Show, Eq)

    type BBlockHeader MainBlockchain = BlockHeader
    type ExtraHeaderData MainBlockchain = MainExtraHeaderData

    -- | In our cryptocurrency, body consists of payloads of all block
    -- components.
    data Body MainBlockchain = MainBody
        { -- | Txp payload.
          _mbTxPayload :: !TxPayload
        , -- | Ssc payload.
          _mbSscPayload :: !SscPayload
        , -- | Heavyweight delegation payload (no-ttl certificates).
          _mbDlgPayload :: !DlgPayload
          -- | Additional update information for the update system.
        , _mbUpdatePayload :: !UpdatePayload
        } deriving (Eq, Show, Generic, Typeable)

    type ExtraBodyData MainBlockchain = MainExtraBodyData
    type BBlock MainBlockchain = Block

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }

deriving instance Show MainToSign
deriving instance Eq MainToSign

instance NFData (BodyProof MainBlockchain)
instance NFData (ConsensusData MainBlockchain)
instance NFData (Body MainBlockchain)
instance NFData MainBlock
