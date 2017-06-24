{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

-- | Lenses for main blockchain types.
--
-- Lenses whose name starts with `mainBlock' are from 'MainBlock' to
-- small parts of it. It makes it clear what exactly is stored in
-- 'MainBlock'. Similar fact is true for `mainHeader' prefix.

module Pos.Block.Core.Main.Lens
       (
         -- * MainToSign
         msHeaderHash
       , msBodyProof
       , msSlot
       , msChainDiff
       , msExtraHeader

         -- * Extra types
       , mehBlockVersion
       , mehSoftwareVersion
       , mehAttributes
       , mebAttributes

         -- * MainConsensusData
       , mcdSlot
       , mcdLeaderKey
       , mcdDifficulty
       , mcdSignature

         -- * MainBlockHeader
       , mainHeaderPrevBlock
       , mainHeaderProof
       , mainHeaderSlot
       , mainHeaderLeaderKey
       , mainHeaderDifficulty
       , mainHeaderSignature
       , mainHeaderBlockVersion
       , mainHeaderSoftwareVersion
       , mainHeaderAttributes

         -- * MainBody
       , mbSscPayload
       , mbTxPayload
       , mbDlgPayload
       , mbUpdatePayload
       , mbTxs
       , mbWitnesses
       , mbTxAddrDistributions

         -- * MainBlock
       , mainBlockPrevBlock
       , mainBlockProof
       , mainBlockSlot
       , mainBlockLeaderKey
       , mainBlockDifficulty
       , mainBlockSignature
       , mainBlockBlockVersion
       , mainBlockSoftwareVersion
       , mainBlockHeaderAttributes
       , mainBlockEBDataProof
       , mainBlockTxPayload
       , mainBlockSscPayload
       , mainBlockDlgPayload
       , mainBlockUpdatePayload
       , mainBlockAttributes
       ) where

import           Universum

import           Control.Lens              (makeLenses)

import           Pos.Block.Core.Main.Chain (Body (..), BodyProof (..), ConsensusData (..))
import           Pos.Block.Core.Main.Types (BlockBodyAttributes, BlockHeaderAttributes,
                                            BlockSignature, MainBlock, MainBlockHeader,
                                            MainBlockchain, MainExtraBodyData,
                                            MainExtraHeaderData, MainToSign (..))
import           Pos.Core                  (BlockVersion, ChainDifficulty, HeaderHash,
                                            SlotId, SoftwareVersion, gbBody, gbExtra,
                                            gbHeader, gbPrevBlock, gbhBodyProof,
                                            gbhConsensus, gbhExtra, gbhPrevBlock)
import           Pos.Crypto                (PublicKey, Hash)
import           Pos.Delegation.Types      (DlgPayload)
import           Pos.Merkle                (MerkleTree)
import           Pos.Ssc.Class.Types       (Ssc (..))
import           Pos.Txp.Core              (Tx, TxDistribution, TxPayload, TxWitness,
                                            txpDistributions, txpTxs, txpWitnesses)
import           Pos.Update.Core.Types     (UpdatePayload)

----------------------------------------------------------------------------
-- MainToSign
----------------------------------------------------------------------------

makeLenses ''MainToSign

----------------------------------------------------------------------------
-- Extra types
----------------------------------------------------------------------------

makeLenses ''MainExtraHeaderData
makeLenses ''MainExtraBodyData

----------------------------------------------------------------------------
-- MainConsensusData
----------------------------------------------------------------------------

makeLenses 'MainConsensusData

----------------------------------------------------------------------------
-- MainBlockHeader
----------------------------------------------------------------------------

-- | Lens from 'MainBlockHeader' to 'HeaderHash' of its parent.
mainHeaderPrevBlock :: Lens' (MainBlockHeader ssc) HeaderHash
mainHeaderPrevBlock = gbhPrevBlock

-- | Lens from 'MainBlockHeader' to 'MainProof'.
mainHeaderProof ::
       Lens' (MainBlockHeader ssc) (BodyProof $ MainBlockchain ssc)
mainHeaderProof = gbhBodyProof

-- | Lens from 'MainBlockHeader' to 'SlotId'.
mainHeaderSlot :: Lens' (MainBlockHeader ssc) SlotId
mainHeaderSlot = gbhConsensus . mcdSlot

-- | Lens from 'MainBlockHeader' to 'PublicKey'.
mainHeaderLeaderKey :: Lens' (MainBlockHeader ssc) PublicKey
mainHeaderLeaderKey = gbhConsensus . mcdLeaderKey

-- | Lens from 'MainBlockHeader' to 'ChainDifficulty'.
mainHeaderDifficulty :: Lens' (MainBlockHeader ssc) ChainDifficulty
mainHeaderDifficulty = gbhConsensus . mcdDifficulty

-- | Lens from 'MainBlockHeader' to 'Signature'.
mainHeaderSignature :: Lens' (MainBlockHeader ssc) (BlockSignature ssc)
mainHeaderSignature = gbhConsensus . mcdSignature

-- | Lens from 'MainBlockHeader' to 'BlockVersion'.
mainHeaderBlockVersion ::
       Lens' (MainBlockHeader ssc) BlockVersion
mainHeaderBlockVersion = gbhExtra . mehBlockVersion

-- | Lens from 'MainBlockHeader' to 'SoftwareVersion'.
mainHeaderSoftwareVersion ::
       Lens' (MainBlockHeader ssc) SoftwareVersion
mainHeaderSoftwareVersion = gbhExtra . mehSoftwareVersion

-- | Lens from 'MainBlockHeader' to 'BlockHeaderAttributes'.
mainHeaderAttributes ::
       Lens' (MainBlockHeader ssc) BlockHeaderAttributes
mainHeaderAttributes = gbhExtra . mehAttributes

-- | Lens from 'MainBlockHeader' to 'MainExtraBodyData'
mainHeaderEBDataProof ::
       Lens' (MainBlockHeader ssc) (Hash MainExtraBodyData)
mainHeaderEBDataProof = gbhExtra . mehEBDataProof

----------------------------------------------------------------------------
-- MainBody
----------------------------------------------------------------------------

makeLenses 'MainBody

-- | Lens for transaction tree in main block body.
mbTxs :: Lens' (Body (MainBlockchain ssc)) (MerkleTree Tx)
mbTxs = mbTxPayload . txpTxs

-- | Lens for witness list in main block body.
mbWitnesses :: Lens' (Body (MainBlockchain ssc)) [TxWitness]
mbWitnesses = mbTxPayload . txpWitnesses

-- | Lens for distributions list in main block body.
mbTxAddrDistributions :: Lens' (Body (MainBlockchain ssc)) [TxDistribution]
mbTxAddrDistributions = mbTxPayload . txpDistributions

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

-- | Lens from 'MainBlock' to 'HeaderHash' of its parent.
mainBlockPrevBlock :: Lens' (MainBlock ssc) HeaderHash
mainBlockPrevBlock = gbPrevBlock

-- | Lens from 'MainBlock' to 'MainProof'.
mainBlockProof :: Lens' (MainBlock ssc) (BodyProof $ MainBlockchain ssc)
mainBlockProof = gbHeader . mainHeaderProof

-- | Lens from 'MainBlock' to 'SlotId'.
mainBlockSlot :: Lens' (MainBlock ssc) SlotId
mainBlockSlot = gbHeader . mainHeaderSlot

-- | Lens from 'MainBlock' to 'PublicKey'.
mainBlockLeaderKey :: Lens' (MainBlock ssc) PublicKey
mainBlockLeaderKey = gbHeader . mainHeaderLeaderKey

-- | Lens from 'MainBlock' to 'ChainDifficulty'.
mainBlockDifficulty :: Lens' (MainBlock ssc) ChainDifficulty
mainBlockDifficulty = gbHeader . mainHeaderDifficulty

-- | Lens from 'MainBlock' to 'Signature'.
mainBlockSignature :: Lens' (MainBlock ssc) (BlockSignature ssc)
mainBlockSignature = gbHeader . mainHeaderSignature

-- | Lens from 'MainBlock' to 'BlockVersion'.
mainBlockBlockVersion ::
       Lens' (MainBlock ssc) BlockVersion
mainBlockBlockVersion = gbHeader . mainHeaderBlockVersion

-- | Lens from 'MainBlock' to 'SoftwareVersion'.
mainBlockSoftwareVersion ::
       Lens' (MainBlock ssc) SoftwareVersion
mainBlockSoftwareVersion = gbHeader . mainHeaderSoftwareVersion

-- | Lens from 'MainBlock' to 'BlockHeaderAttributes'.
mainBlockHeaderAttributes ::
       Lens' (MainBlock ssc) BlockHeaderAttributes
mainBlockHeaderAttributes = gbHeader . mainHeaderAttributes

-- | Lens from 'MainBlock' to 'BlockHeaderAttributes'.
mainBlockEBDataProof ::
       Lens' (MainBlock ssc) (Hash MainExtraBodyData)
mainBlockEBDataProof = gbHeader . mainHeaderEBDataProof

-- | Lens from 'MainBlock' to 'TxPayload'.
mainBlockTxPayload :: Lens' (MainBlock ssc) TxPayload
mainBlockTxPayload = gbBody . mbTxPayload

-- | Lens from 'MainBlock' to 'SscPayload'.
mainBlockSscPayload :: Lens' (MainBlock ssc) (SscPayload ssc)
mainBlockSscPayload = gbBody . mbSscPayload

-- | Lens from 'MainBlock' to 'UpdatePayload'.
mainBlockUpdatePayload :: Lens' (MainBlock ssc) UpdatePayload
mainBlockUpdatePayload = gbBody . mbUpdatePayload

-- | Lens from 'MainBlock' to 'DlgPayload'.
mainBlockDlgPayload :: Lens' (MainBlock ssc) DlgPayload
mainBlockDlgPayload = gbBody . mbDlgPayload

-- | Lens from 'MainBlock' to 'BlockBodyAttributes'.
mainBlockAttributes :: Lens' (MainBlock ssc) BlockBodyAttributes
mainBlockAttributes = gbExtra . mebAttributes
