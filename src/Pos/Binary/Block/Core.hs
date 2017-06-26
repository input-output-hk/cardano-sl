-- | Serialization of core types from 'Pos.Block'.

module Pos.Binary.Block.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class             (Bi (..), Cons (..), Field (..),
                                               convertToSizeNPut, deriveSimpleBi,
                                               getWord8, label, labelS, putField, putS,
                                               putWord8S)
import           Pos.Binary.Core              ()
import           Pos.Binary.Txp               ()
import           Pos.Binary.Update            ()
import           Pos.Crypto                   (Hash)
import qualified Pos.Block.Core.Genesis.Chain as BC
import qualified Pos.Block.Core.Genesis.Types as BC
import qualified Pos.Block.Core.Main.Chain    as BC
import qualified Pos.Block.Core.Main.Types    as BC
import           Pos.Core                     (BlockVersion, SoftwareVersion)
import qualified Pos.Core.Block               as Core
import           Pos.Ssc.Class.Types          (Ssc (..))

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Ssc ssc =>
         Bi (Core.BodyProof (BC.MainBlockchain ssc)) where
    sizeNPut = labelS "MainProof" $
        putField BC.mpTxProof <>
        putField BC.mpMpcProof <>
        putField BC.mpProxySKsProof <>
        putField BC.mpUpdateProof
    get = label "MainProof" $ BC.MainProof <$> get <*> get <*> get <*> get

instance Bi (BC.BlockSignature ssc) where
    sizeNPut = labelS "BlockSignature" $ convertToSizeNPut f
      where
        f (BC.BlockSignature sig)            = putWord8S 0 <> putS sig
        f (BC.BlockPSignatureLight proxySig) = putWord8S 1 <> putS proxySig
        f (BC.BlockPSignatureHeavy proxySig) = putWord8S 2 <> putS proxySig
    get = label "BlockSignature" $ getWord8 >>= \case
        0 -> BC.BlockSignature <$> get
        1 -> BC.BlockPSignatureLight <$> get
        2 -> BC.BlockPSignatureHeavy <$> get
        t -> fail $ "get@BlockSignature: unknown tag: " <> show t

instance Bi (BC.ConsensusData (BC.MainBlockchain ssc)) where
    sizeNPut = labelS "MainConsensusData" $
        putField BC._mcdSlot <>
        putField BC._mcdLeaderKey <>
        putField BC._mcdDifficulty <>
        putField BC._mcdSignature
    get = label "MainConsensusData" $ BC.MainConsensusData <$> get <*> get <*> get <*> get

instance (Ssc ssc) => Bi (BC.Body (BC.MainBlockchain ssc)) where
    sizeNPut = labelS "MainBody" $
        putField BC._mbTxPayload <>
        putField BC._mbSscPayload <>
        putField BC._mbDlgPayload <>
        putField BC._mbUpdatePayload
    get = label "MainBody" $ do
        _mbTxPayload     <- get
        _mbSscPayload    <- get
        _mbDlgPayload    <- get
        _mbUpdatePayload <- get
        return BC.MainBody{..}

deriveSimpleBi ''BC.MainExtraHeaderData [
    Cons 'BC.MainExtraHeaderData [
        Field [| BC._mehBlockVersion    :: BlockVersion              |],
        Field [| BC._mehSoftwareVersion :: SoftwareVersion           |],
        Field [| BC._mehAttributes      :: BC.BlockHeaderAttributes  |],
        Field [| BC._mehEBDataProof     :: Hash BC.MainExtraBodyData |]
    ]]

deriveSimpleBi ''BC.MainExtraBodyData [
    Cons 'BC.MainExtraBodyData [
        Field [| BC._mebAttributes :: BC.BlockBodyAttributes |]
    ]]

instance Ssc ssc => Bi (BC.MainToSign ssc) where
    sizeNPut = labelS "MainToSign" $
        putField BC._msHeaderHash <>
        putField BC._msBodyProof <>
        putField BC._msSlot <>
        putField BC._msChainDiff <>
        putField BC._msExtraHeader
    get = label "MainToSign" $ BC.MainToSign <$> get <*> get <*> get <*> get <*> get

-- ----------------------------------------------------------------------------
-- -- GenesisBlock
-- ----------------------------------------------------------------------------

deriveSimpleBi ''BC.GenesisExtraHeaderData [
    Cons 'BC.GenesisExtraHeaderData [
        Field [| BC._gehAttributes :: BC.GenesisHeaderAttributes |]
    ]]

deriveSimpleBi ''BC.GenesisExtraBodyData [
    Cons 'BC.GenesisExtraBodyData [
        Field [| BC._gebAttributes :: BC.GenesisBodyAttributes |]
    ]]

instance Bi (BC.BodyProof (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisProof" $ putField (\(BC.GenesisProof h) -> h)
    get = label "GenesisProof" $ BC.GenesisProof <$> get

instance Bi (BC.ConsensusData (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisConsensusData" $
        putField BC._gcdEpoch <>
        putField BC._gcdDifficulty
    get = label "GenesisConsensusData" $ BC.GenesisConsensusData <$> get <*> get

instance Bi (BC.Body (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisBody" $ putField BC._gbLeaders
    get = label "GenesisBody" $ BC.GenesisBody <$> get
