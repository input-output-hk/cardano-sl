{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | SafeCopy serialization of Pos.Types.* modules

module Pos.SafeCopy.Types
       (
       ) where

import           Data.SafeCopy         (SafeCopy (..), base, contain,
                                        deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Serialize        as Cereal (getWord8, putWord8)
import           Universum

import           Pos.Ssc.Class.Types   (Ssc (..))
-- FIXME
import           Pos.Core.Types
import           Pos.Txp.Core.Types
import           Pos.Types.Block
import           Pos.Update.Core.Types

deriveSafeCopySimple 0 'base ''CoinPortion
deriveSafeCopySimple 0 'base ''EpochIndex
deriveSafeCopySimple 0 'base ''LocalSlotIndex
deriveSafeCopySimple 0 'base ''SlotId
deriveSafeCopySimple 0 'base ''EpochOrSlot
deriveSafeCopySimple 0 'base ''Coin
deriveSafeCopySimple 0 'base ''AddrPkAttrs
deriveSafeCopySimple 0 'base ''Address
deriveSafeCopySimple 0 'base ''TxInWitness
-- TODO: in many cases TxDistribution would just be lots of empty lists, so
-- its SafeCopy instance could be optimised
deriveSafeCopySimple 0 'base ''TxDistribution
deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''TxOutAux
deriveSafeCopySimple 0 'base ''Tx
deriveSafeCopySimple 0 'base ''TxProof
deriveSafeCopySimple 0 'base ''TxPayload
deriveSafeCopySimple 0 'base ''SharedSeed

deriveSafeCopySimple 0 'base ''MainExtraBodyData
deriveSafeCopySimple 0 'base ''MainExtraHeaderData

deriveSafeCopySimple 0 'base ''SystemTag
deriveSafeCopySimple 0 'base ''UpdateData
deriveSafeCopySimple 0 'base ''BlockVersionData
deriveSafeCopySimple 0 'base ''UpdateProposal
deriveSafeCopySimple 0 'base ''UpdateVote
deriveSafeCopySimple 0 'base ''UpdatePayload

-- Manually written instances can't be derived because
-- 'deriveSafeCopySimple' is not clever enough to add
-- “SafeCopy (Whatever a) =>” constaints.
instance ( SafeCopy (BHeaderHash b)
         , SafeCopy (BodyProof b)
         , SafeCopy (ConsensusData b)
         , SafeCopy (ExtraHeaderData b)
         ) =>
         SafeCopy (GenericBlockHeader b) where
    getCopy =
        contain $
        do _gbhPrevBlock <- safeGet
           _gbhBodyProof <- safeGet
           _gbhConsensus <- safeGet
           _gbhExtra <- safeGet
           return $! GenericBlockHeader {..}
    putCopy GenericBlockHeader {..} =
        contain $
        do safePut _gbhPrevBlock
           safePut _gbhBodyProof
           safePut _gbhConsensus
           safePut _gbhExtra

instance ( SafeCopy (BHeaderHash b)
         , SafeCopy (BodyProof b)
         , SafeCopy (ConsensusData b)
         , SafeCopy (ExtraHeaderData b)
         , SafeCopy (Body b)
         , SafeCopy (ExtraBodyData b)
         ) =>
         SafeCopy (GenericBlock b) where
    getCopy =
        contain $
        do _gbHeader <- safeGet
           _gbBody <- safeGet
           _gbExtra <- safeGet
           return $! GenericBlock {..}
    putCopy GenericBlock {..} =
        contain $
        do safePut _gbHeader
           safePut _gbBody
           safePut _gbExtra

deriveSafeCopySimple 0 'base ''ChainDifficulty

instance (Ssc ssc, SafeCopy (SscProof ssc)) =>
         SafeCopy (BodyProof (MainBlockchain ssc)) where
    getCopy = contain $ do
        mpTxProof <- safeGet
        mpMpcProof      <- safeGet
        mpProxySKsProof <- safeGet
        mpUpdateProof   <- safeGet
        return $! MainProof{..}
    putCopy MainProof {..} = contain $ do
        safePut mpTxProof
        safePut mpMpcProof
        safePut mpProxySKsProof
        safePut mpUpdateProof

instance SafeCopy (BodyProof (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do x <- safeGet
           return $! GenesisProof x
    putCopy (GenesisProof x) =
        contain $
        do safePut x

instance SafeCopy (BlockSignature ssc) where
    getCopy = contain $ Cereal.getWord8 >>= \case
        0 -> BlockSignature <$> safeGet
        1 -> BlockPSignatureEpoch <$> safeGet
        2 -> BlockPSignatureSimple <$> safeGet
        t -> fail $ "getCopy@BlockSignature: couldn't read tag: " <> show t
    putCopy (BlockSignature sig)       = contain $ Cereal.putWord8 0 >> safePut sig
    putCopy (BlockPSignatureEpoch proxySig) = contain $ Cereal.putWord8 1 >> safePut proxySig
    putCopy (BlockPSignatureSimple proxySig) = contain $ Cereal.putWord8 2 >> safePut proxySig

instance SafeCopy (ConsensusData (MainBlockchain ssc)) where
    getCopy =
        contain $
        do _mcdSlot <- safeGet
           _mcdLeaderKey <- safeGet
           _mcdDifficulty <- safeGet
           _mcdSignature <- safeGet
           return $! MainConsensusData {..}
    putCopy MainConsensusData {..} =
        contain $
        do safePut _mcdSlot
           safePut _mcdLeaderKey
           safePut _mcdDifficulty
           safePut _mcdSignature

instance SafeCopy (ConsensusData (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do _gcdEpoch <- safeGet
           _gcdDifficulty <- safeGet
           return $! GenesisConsensusData {..}
    putCopy GenesisConsensusData {..} =
        contain $
        do safePut _gcdEpoch
           safePut _gcdDifficulty

instance (Ssc ssc, SafeCopy (SscPayload ssc)) =>
         SafeCopy (Body (MainBlockchain ssc)) where
    getCopy = contain $ do
        _mbTxPayload     <- safeGet
        _mbMpc           <- safeGet
        _mbProxySKs      <- safeGet
        _mbUpdatePayload <- safeGet
        return $! MainBody{..}
    putCopy MainBody {..} = contain $ do
        safePut _mbTxPayload
        safePut _mbMpc
        safePut _mbProxySKs
        safePut _mbUpdatePayload

instance SafeCopy (Body (GenesisBlockchain ssc)) where
    getCopy =
        contain $
        do _gbLeaders <- safeGet
           return $! GenesisBody {..}
    putCopy GenesisBody {..} =
        contain $
        do safePut _gbLeaders
