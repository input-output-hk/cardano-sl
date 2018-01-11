-- | Serialization of core types from 'Pos.Core.Block'.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi, encodeListLen,
                                   enforceSize)
import           Pos.Binary.Core.Txp ()
import qualified Pos.Core.Block.Blockchain as Core
import qualified Pos.Core.Block.Genesis.Chain as BC
import qualified Pos.Core.Block.Genesis.Types as BC
import qualified Pos.Core.Block.Main.Chain as BC
import qualified Pos.Core.Block.Main.Types as BC
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash)

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Bi (Core.BodyProof BC.MainBlockchain) where
    encode bc =  encodeListLen 4
              <> encode (BC.mpTxProof bc)
              <> encode (BC.mpMpcProof bc)
              <> encode (BC.mpProxySKsProof bc)
              <> encode (BC.mpUpdateProof bc)
    decode = do
        enforceSize "Core.BodyProof BC.MainBlockChain" 4
        BC.MainProof <$> decode <*>
                         decode <*>
                         decode <*>
                         decode

instance HasConfiguration => Bi BC.BlockSignature where
    encode input = case input of
        BC.BlockSignature sig       -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
        BC.BlockPSignatureLight pxy -> encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
        BC.BlockPSignatureHeavy pxy -> encodeListLen 2 <> encode (2 :: Word8) <> encode pxy
    decode = do
        enforceSize "BlockSignature" 2
        tag <- decode @Word8
        case tag of
          0 -> BC.BlockSignature <$> decode
          1 -> BC.BlockPSignatureLight <$> decode
          2 -> BC.BlockPSignatureHeavy <$> decode
          _ -> fail $ "decode@BlockSignature: unknown tag: " <> show tag

instance HasConfiguration => Bi (BC.ConsensusData BC.MainBlockchain) where
    encode cd =  encodeListLen 4
              <> encode (BC._mcdSlot cd)
              <> encode (BC._mcdLeaderKey cd)
              <> encode (BC._mcdDifficulty cd)
              <> encode (BC._mcdSignature cd)
    decode = do
        enforceSize "BC.ConsensusData BC.MainBlockchain)" 4
        BC.MainConsensusData <$> decode <*>
                                 decode <*>
                                 decode <*>
                                 decode

instance HasConfiguration => Bi (BC.Body BC.MainBlockchain) where
    encode bc =  encodeListLen 4
              <> encode (BC._mbTxPayload  bc)
              <> encode (BC._mbSscPayload bc)
              <> encode (BC._mbDlgPayload bc)
              <> encode (BC._mbUpdatePayload bc)
    decode = do
        enforceSize "BC.Body BC.MainBlockchain" 4
        BC.MainBody <$> decode <*>
                        decode <*>
                        decode <*>
                        decode

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

instance HasConfiguration => Bi BC.MainToSign where
    encode mts = encodeListLen 5
               <> encode (BC._msHeaderHash mts)
               <> encode (BC._msBodyProof mts)
               <> encode (BC._msSlot mts)
               <> encode (BC._msChainDiff mts)
               <> encode (BC._msExtraHeader mts)
    decode = do
        enforceSize "BC.MainToSign" 5
        BC.MainToSign <$> decode <*>
                          decode <*>
                          decode <*>
                          decode <*>
                          decode

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

instance Bi (BC.BodyProof BC.GenesisBlockchain) where
    encode (BC.GenesisProof h) = encode h
    decode = BC.GenesisProof <$> decode

instance Bi (BC.ConsensusData BC.GenesisBlockchain) where
    encode bc =  encodeListLen 2
              <> encode (BC._gcdEpoch bc)
              <> encode (BC._gcdDifficulty bc)
    decode = do
      enforceSize "BC.ConsensusData BC.GenesisBlockchain" 2
      BC.GenesisConsensusData <$> decode <*> decode

instance Bi (BC.Body BC.GenesisBlockchain) where
    encode = encode . BC._gbLeaders
    decode = BC.GenesisBody <$> decode
