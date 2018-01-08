-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Binary.Core.Common ()
import qualified Pos.Core.Block.Blockchain as T
import           Pos.Crypto.Configuration (HasCryptoConfiguration, getProtocolMagic, protocolMagic)

instance ( Typeable b
         , Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , T.BlockchainHelpers b
         , HasCryptoConfiguration
         ) =>
         Bi (T.GenericBlockHeader b) where
    encode bh =  encodeListLen 5
              <> encode (getProtocolMagic protocolMagic)
              <> {-# SCC "encode_header_prev" #-} encode (T._gbhPrevBlock bh)
              <> {-# SCC "encode_header_body_proof" #-} encode (T._gbhBodyProof bh)
              <> {-# SCC "encode_header_consensus" #-} encode (T._gbhConsensus bh)
              <> {-# SCC "encode_header_extra" #-} encode (T._gbhExtra bh)
    decode = do
        enforceSize "GenericBlockHeader b" 5
        blockMagic <- {-# SCC "decode_header_magic" #-} decode
        when (blockMagic /= getProtocolMagic protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        _gbhPrevBlock <- {-# SCC "decode_header_prev" #-} decode
        _gbhBodyProof <- {-# SCC "decode_header_body_proof" #-} decode
        _gbhConsensus <- {-# SCC "decode_header_consensus" #-} decode
        _gbhExtra     <- {-# SCC "decode_header_extra" #-} decode
        pure T.UnsafeGenericBlockHeader {..}

instance ( Typeable b
         , Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         , T.BlockchainHelpers b
         , HasCryptoConfiguration
         ) =>
         Bi (T.GenericBlock b) where
    encode gb =  encodeListLen 3
              <> {-# SCC "encode_block_header" #-} encode (T._gbHeader gb)
              <> {-# SCC "encode_block_body" #-} encode (T._gbBody gb)
              <> {-# SCC "encode_block_extra" #-} encode (T._gbExtra gb)
    decode = do
        enforceSize "GenericBlock" 3
        _gbHeader <- {-# SCC "decode_block_header" #-} decode
        _gbBody   <- {-# SCC "decode_block_body" #-} decode
        _gbExtra  <- {-# SCC "decode_block_extra" #-} decode
        pure T.UnsafeGenericBlock {..}
