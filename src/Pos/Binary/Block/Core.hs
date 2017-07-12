-- | Serialization of core types from 'Pos.Block'.

module Pos.Binary.Block.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class             (Bi (..), Cons (..), Field (..),
                                               convertToSizeNPut, deriveSimpleBi,
                                               getWord8, label, labelS, putField, putS,
                                               putWord8S)
import qualified Pos.Binary.Cbor              as Cbor
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

instance Ssc ssc => Cbor.Bi (Core.BodyProof (BC.MainBlockchain ssc)) where
  encode bc =  Cbor.encodeListLen 4
            <> Cbor.encode (BC.mpTxProof bc)
            <> Cbor.encode (BC.mpMpcProof bc)
            <> Cbor.encode (BC.mpProxySKsProof bc)
            <> Cbor.encode (BC.mpUpdateProof bc)
  decode = do
    Cbor.enforceSize "Core.BodyProof (BC.MainBlockChain ssc)" 4
    BC.MainProof <$> Cbor.decode <*>
                     Cbor.decode <*>
                     Cbor.decode <*>
                     Cbor.decode

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

instance Cbor.Bi (BC.BlockSignature ssc) where
{--
    = BlockSignature (Signature (MainToSign ssc))
    | BlockPSignatureLight (ProxySigLight (MainToSign ssc))
    | BlockPSignatureHeavy (ProxySigHeavy (MainToSign ssc))
--}
  encode input = case input of
    BC.BlockSignature sig       -> Cbor.encodeListLen 2 <> Cbor.encode (0 :: Word8) <> Cbor.encode sig
    BC.BlockPSignatureLight pxy -> Cbor.encodeListLen 2 <> Cbor.encode (1 :: Word8) <> Cbor.encode pxy
    BC.BlockPSignatureHeavy pxy -> Cbor.encodeListLen 2 <> Cbor.encode (2 :: Word8) <> Cbor.encode pxy
  decode = do
    Cbor.enforceSize "BlockSignature" 2
    tag <- Cbor.decode @Word8
    case tag of
      0 -> BC.BlockSignature <$> Cbor.decode
      1 -> BC.BlockPSignatureLight <$> Cbor.decode
      2 -> BC.BlockPSignatureHeavy <$> Cbor.decode
      _ -> fail $ "decode@BlockSignature: unknown tag: " <> show tag

instance Bi (BC.ConsensusData (BC.MainBlockchain ssc)) where
    sizeNPut = labelS "MainConsensusData" $
        putField BC._mcdSlot <>
        putField BC._mcdLeaderKey <>
        putField BC._mcdDifficulty <>
        putField BC._mcdSignature
    get = label "MainConsensusData" $ BC.MainConsensusData <$> get <*> get <*> get <*> get

instance Cbor.Bi (BC.ConsensusData (BC.MainBlockchain ssc)) where
  encode cd =  Cbor.encodeListLen 4
            <> Cbor.encode (BC._mcdSlot cd)
            <> Cbor.encode (BC._mcdLeaderKey cd)
            <> Cbor.encode (BC._mcdDifficulty cd)
            <> Cbor.encode (BC._mcdSignature cd)
  decode = do
    Cbor.enforceSize "BC.ConsensusData (BC.MainBlockchain ssc))" 4
    BC.MainConsensusData <$> Cbor.decode <*>
                             Cbor.decode <*>
                             Cbor.decode <*>
                             Cbor.decode

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

instance (Ssc ssc) => Cbor.Bi (BC.Body (BC.MainBlockchain ssc)) where
  encode bc =  Cbor.encodeListLen 4
            <> Cbor.encode (BC._mbTxPayload  bc)
            <> Cbor.encode (BC._mbSscPayload bc)
            <> Cbor.encode (BC._mbDlgPayload bc)
            <> Cbor.encode (BC._mbUpdatePayload bc)
  decode = do
    Cbor.enforceSize "BC.Body (BC.MainBlockchain ssc)" 4
    BC.MainBody <$> Cbor.decode <*>
                    Cbor.decode <*>
                    Cbor.decode <*>
                    Cbor.decode

deriveSimpleBi ''BC.MainExtraHeaderData [
    Cons 'BC.MainExtraHeaderData [
        Field [| BC._mehBlockVersion    :: BlockVersion              |],
        Field [| BC._mehSoftwareVersion :: SoftwareVersion           |],
        Field [| BC._mehAttributes      :: BC.BlockHeaderAttributes  |],
        Field [| BC._mehEBDataProof     :: Hash BC.MainExtraBodyData |]
    ]]

Cbor.deriveSimpleBi ''BC.MainExtraHeaderData [
    Cbor.Cons 'BC.MainExtraHeaderData [
        Cbor.Field [| BC._mehBlockVersion    :: BlockVersion              |],
        Cbor.Field [| BC._mehSoftwareVersion :: SoftwareVersion           |],
        Cbor.Field [| BC._mehAttributes      :: BC.BlockHeaderAttributes  |],
        Cbor.Field [| BC._mehEBDataProof     :: Hash BC.MainExtraBodyData |]
    ]]

deriveSimpleBi ''BC.MainExtraBodyData [
    Cons 'BC.MainExtraBodyData [
        Field [| BC._mebAttributes :: BC.BlockBodyAttributes |]
    ]]

Cbor.deriveSimpleBi ''BC.MainExtraBodyData [
    Cbor.Cons 'BC.MainExtraBodyData [
        Cbor.Field [| BC._mebAttributes :: BC.BlockBodyAttributes |]
    ]]

instance Ssc ssc => Bi (BC.MainToSign ssc) where
    sizeNPut = labelS "MainToSign" $
        putField BC._msHeaderHash <>
        putField BC._msBodyProof <>
        putField BC._msSlot <>
        putField BC._msChainDiff <>
        putField BC._msExtraHeader
    get = label "MainToSign" $ BC.MainToSign <$> get <*> get <*> get <*> get <*> get

instance Ssc ssc => Cbor.Bi (BC.MainToSign ssc) where
  encode mts = Cbor.encodeListLen 5
             <> Cbor.encode (BC._msHeaderHash mts)
             <> Cbor.encode (BC._msBodyProof mts)
             <> Cbor.encode (BC._msSlot mts)
             <> Cbor.encode (BC._msChainDiff mts)
             <> Cbor.encode (BC._msExtraHeader mts)
  decode = do
    Cbor.enforceSize "BC.MainToSign" 5
    BC.MainToSign <$> Cbor.decode <*>
                      Cbor.decode <*>
                      Cbor.decode <*>
                      Cbor.decode <*>
                      Cbor.decode

-- ----------------------------------------------------------------------------
-- -- GenesisBlock
-- ----------------------------------------------------------------------------

deriveSimpleBi ''BC.GenesisExtraHeaderData [
    Cons 'BC.GenesisExtraHeaderData [
        Field [| BC._gehAttributes :: BC.GenesisHeaderAttributes |]
    ]]

Cbor.deriveSimpleBi ''BC.GenesisExtraHeaderData [
    Cbor.Cons 'BC.GenesisExtraHeaderData [
        Cbor.Field [| BC._gehAttributes :: BC.GenesisHeaderAttributes |]
    ]]

deriveSimpleBi ''BC.GenesisExtraBodyData [
    Cons 'BC.GenesisExtraBodyData [
        Field [| BC._gebAttributes :: BC.GenesisBodyAttributes |]
    ]]

Cbor.deriveSimpleBi ''BC.GenesisExtraBodyData [
    Cbor.Cons 'BC.GenesisExtraBodyData [
        Cbor.Field [| BC._gebAttributes :: BC.GenesisBodyAttributes |]
    ]]

instance Bi (BC.BodyProof (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisProof" $ putField (\(BC.GenesisProof h) -> h)
    get = label "GenesisProof" $ BC.GenesisProof <$> get

instance Cbor.Bi (BC.BodyProof (BC.GenesisBlockchain ssc)) where
  encode (BC.GenesisProof h) = Cbor.encode h
  decode = BC.GenesisProof <$> Cbor.decode

instance Bi (BC.ConsensusData (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisConsensusData" $
        putField BC._gcdEpoch <>
        putField BC._gcdDifficulty
    get = label "GenesisConsensusData" $ BC.GenesisConsensusData <$> get <*> get

instance Cbor.Bi (BC.ConsensusData (BC.GenesisBlockchain ssc)) where
  encode bc =  Cbor.encodeListLen 2
            <> Cbor.encode (BC._gcdEpoch bc)
            <> Cbor.encode (BC._gcdDifficulty bc)
  decode = do
    Cbor.enforceSize "BC.ConsensusData (BC.GenesisBlockchain ssc)" 2
    BC.GenesisConsensusData <$> Cbor.decode <*> Cbor.decode

instance Bi (BC.Body (BC.GenesisBlockchain ssc)) where
    sizeNPut = labelS "GenesisBody" $ putField BC._gbLeaders
    get = label "GenesisBody" $ BC.GenesisBody <$> get

instance Cbor.Bi (BC.Body (BC.GenesisBlockchain ssc)) where
  encode = Cbor.encode . BC._gbLeaders
  decode = BC.GenesisBody <$> Cbor.decode
