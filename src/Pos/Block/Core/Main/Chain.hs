{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Definitions of the main blockchain ('Blockchain' class and related).

module Pos.Block.Core.Main.Chain
       ( BodyProof (..)
       , ConsensusData (..)
       , Body (..)
       ) where

import           Universum

import           Data.Tagged                (untag)

import           Pos.Binary.Class           (Bi)
import           Pos.Binary.Core            ()
import           Pos.Binary.Txp             ()
import           Pos.Binary.Update          ()
import           Pos.Block.Core.Main.Types  (MainBlock, MainBlockchain, MainExtraBodyData,
                                             MainExtraHeaderData)
import           Pos.Block.Core.Union.Types (BiHeader, Block, BlockHeader,
                                             BlockSignature (..))
import           Pos.Core                   (Blockchain (..), ChainDifficulty,
                                             GenericBlockHeader (..), IsMainHeader (..),
                                             ProxySKHeavy, SlotId (..))
import           Pos.Crypto                 (Hash, PublicKey, hash)
import           Pos.Delegation.Types       (DlgPayload)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.Txp.Core               (TxPayload, TxProof, mkTxProof)
import           Pos.Update.Core.Types      (UpdatePayload, UpdateProof, mkUpdateProof)

instance ( BiHeader ssc
         , Ssc ssc
         , Bi $ BodyProof $ MainBlockchain ssc
         , IsMainHeader (GenericBlockHeader $ MainBlockchain ssc)) =>
         Blockchain (MainBlockchain ssc) where

    -- | Proof of everything contained in the payload.
    data BodyProof (MainBlockchain ssc) = MainProof
        { mpTxProof       :: !TxProof
        , mpMpcProof      :: !(SscProof ssc)
        , mpProxySKsProof :: !(Hash [ProxySKHeavy])
        , mpUpdateProof   :: !UpdateProof
        } deriving (Generic)
    data ConsensusData (MainBlockchain ssc) = MainConsensusData
        { -- | Id of the slot for which this block was generated.
        _mcdSlot       :: !SlotId
        , -- | Public key of the slot leader. It's essential to have it here,
          -- because FTS gives us only hash of public key (aka 'StakeholderId').
        _mcdLeaderKey  :: !PublicKey
        , -- | Difficulty of chain ending in this block.
        _mcdDifficulty :: !ChainDifficulty
        , -- | Signature given by slot leader.
        _mcdSignature  :: !(BlockSignature ssc)
        } deriving (Generic, Show, Eq)
    type BBlockHeader (MainBlockchain ssc) = BlockHeader ssc
    type ExtraHeaderData (MainBlockchain ssc) = MainExtraHeaderData

    -- | In our cryptocurrency, body consists of payloads of all block
    -- components.
    data Body (MainBlockchain ssc) = MainBody
        { -- | Txp payload.
          _mbTxPayload :: !TxPayload
        , -- | Ssc payload.
          _mbSscPayload :: !(SscPayload ssc)
        , -- | Heavyweight delegation payload (no-ttl certificates).
          _mbDlgPayload :: !DlgPayload
          -- | Additional update information for the update system.
        , _mbUpdatePayload :: !UpdatePayload
        } deriving (Generic, Typeable)

    type ExtraBodyData (MainBlockchain ssc) = MainExtraBodyData
    type BBlock (MainBlockchain ssc) = Block ssc

    mkBodyProof MainBody{..} =
        MainProof
        { mpTxProof = mkTxProof _mbTxPayload
        , mpMpcProof = untag @ssc mkSscProof _mbSscPayload
        , mpProxySKsProof = hash _mbDlgPayload
        , mpUpdateProof = mkUpdateProof _mbUpdatePayload
        }

deriving instance Ssc ssc => Show (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Eq (BodyProof (MainBlockchain ssc))
deriving instance Ssc ssc => Show (Body (MainBlockchain ssc))
deriving instance (Eq (SscPayload ssc), Ssc ssc) => Eq (Body (MainBlockchain ssc))

instance (Ssc ssc) => NFData (BodyProof (MainBlockchain ssc))
instance (Ssc ssc) => NFData (ConsensusData (MainBlockchain ssc))
instance (Ssc ssc) => NFData (Body (MainBlockchain ssc))
instance (Ssc ssc) => NFData (MainBlock ssc)
