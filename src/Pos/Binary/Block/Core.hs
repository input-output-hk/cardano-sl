-- | Serialization of core types from 'Pos.Block'.

module Pos.Binary.Block.Core
       (
       ) where

import           Universum

import           Pos.Binary.Class             (Bi (..), Cons (..), Field (..), deriveSimpleBi, encodeListLen,
                                               enforceSize)
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

instance Ssc ssc => Bi (Core.BodyProof (BC.MainBlockchain ssc)) where
  encode bc =  encodeListLen 4
            <> encode (BC.mpTxProof bc)
            <> encode (BC.mpMpcProof bc)
            <> encode (BC.mpProxySKsProof bc)
            <> encode (BC.mpUpdateProof bc)
  decode = do
    enforceSize "Core.BodyProof (BC.MainBlockChain ssc)" 4
    BC.MainProof <$> decode <*>
                     decode <*>
                     decode <*>
                     decode

instance Bi (BC.BlockSignature ssc) where
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

instance Bi (BC.ConsensusData (BC.MainBlockchain ssc)) where
  encode cd =  encodeListLen 4
            <> encode (BC._mcdSlot cd)
            <> encode (BC._mcdLeaderKey cd)
            <> encode (BC._mcdDifficulty cd)
            <> encode (BC._mcdSignature cd)
  decode = do
    enforceSize "BC.ConsensusData (BC.MainBlockchain ssc))" 4
    BC.MainConsensusData <$> decode <*>
                             decode <*>
                             decode <*>
                             decode

instance (Ssc ssc) => Bi (BC.Body (BC.MainBlockchain ssc)) where
  encode bc =  encodeListLen 4
            <> encode (BC._mbTxPayload  bc)
            <> encode (BC._mbSscPayload bc)
            <> encode (BC._mbDlgPayload bc)
            <> encode (BC._mbUpdatePayload bc)
  decode = do
    enforceSize "BC.Body (BC.MainBlockchain ssc)" 4
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

instance Ssc ssc => Bi (BC.MainToSign ssc) where
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

instance Bi (BC.BodyProof (BC.GenesisBlockchain ssc)) where
  encode (BC.GenesisProof h) = encode h
  decode = BC.GenesisProof <$> decode

instance Bi (BC.ConsensusData (BC.GenesisBlockchain ssc)) where
  encode bc =  encodeListLen 2
            <> encode (BC._gcdEpoch bc)
            <> encode (BC._gcdDifficulty bc)
  decode = do
    enforceSize "BC.ConsensusData (BC.GenesisBlockchain ssc)" 2
    BC.GenesisConsensusData <$> decode <*> decode

instance Bi (BC.Body (BC.GenesisBlockchain ssc)) where
  encode = encode . BC._gbLeaders
  decode = BC.GenesisBody <$> decode
