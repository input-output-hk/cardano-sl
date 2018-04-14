-- | Serialization of core types from 'Pos.Core.Block'.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi, encodeListLen,
                                   encodeListLen, enforceSize)
import           Pos.Binary.Core.Txp ()
import qualified Pos.Core.Block.Blockchain as Core
import qualified Pos.Core.Block.Genesis.Chain as BC
import qualified Pos.Core.Block.Genesis.Types as BC
import qualified Pos.Core.Block.Main.Chain as BC
import qualified Pos.Core.Block.Main.Types as BC
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash)
import           Pos.Util.Util (cborError)

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
    encodedSize bc =
        1 + encodedSize (BC.mpTxProof bc)
          + encodedSize (BC.mpTxProof bc)
          + encodedSize (BC.mpMpcProof bc)
          + encodedSize (BC.mpProxySKsProof bc)
          + encodedSize (BC.mpUpdateProof bc)

instance Bi BC.BlockSignature where
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
          _ -> cborError $ "decode@BlockSignature: unknown tag: " <> show tag
    encodedSize input = case input of
        BC.BlockSignature sig       -> 2 + encodedSize sig
        BC.BlockPSignatureLight pxy -> 2 + encodedSize pxy
        BC.BlockPSignatureHeavy pxy -> 2 + encodedSize pxy

instance Bi (BC.ConsensusData BC.MainBlockchain) where
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
    encodedSize cd =
            1 + encodedSize (BC._mcdSlot cd)
              + encodedSize (BC._mcdLeaderKey cd)
              + encodedSize (BC._mcdDifficulty cd)
              + encodedSize (BC._mcdSignature cd)

instance Bi (BC.Body BC.MainBlockchain) where
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
    encodedSize bc =
            1 + encodedSize (BC._mbTxPayload  bc)
              + encodedSize (BC._mbSscPayload bc)
              + encodedSize (BC._mbDlgPayload bc)
              + encodedSize (BC._mbUpdatePayload bc)

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

instance Bi BC.MainToSign where
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
    encodedSize mts =
               1 + encodedSize (BC._msHeaderHash mts)
                 + encodedSize (BC._msBodyProof mts)
                 + encodedSize (BC._msSlot mts)
                 + encodedSize (BC._msChainDiff mts)
                 + encodedSize (BC._msExtraHeader mts)

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
    encodedSize (BC.GenesisProof h) = encodedSize h

instance Bi (BC.ConsensusData BC.GenesisBlockchain) where
    encode bc =  encodeListLen 2
              <> encode (BC._gcdEpoch bc)
              <> encode (BC._gcdDifficulty bc)
    decode = do
      enforceSize "BC.ConsensusData BC.GenesisBlockchain" 2
      BC.GenesisConsensusData <$> decode <*> decode

    encodedSize bc =
            1 + encodedSize (BC._gcdEpoch bc)
              + encodedSize (BC._gcdDifficulty bc)

instance Bi (BC.Body BC.GenesisBlockchain) where
    encode = encode . BC._gbLeaders
    decode = BC.GenesisBody <$> decode
    encodedSize = encodedSize . BC._gbLeaders
