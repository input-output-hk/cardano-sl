-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Universum
import           Codec.CBOR.Encoding (encodeBytes)
import qualified Data.ByteString as BS

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
              <> encode (T._gbhPrevBlock bh)
              <> encode (T._gbhBodyProof bh)
              <> encode (T._gbhConsensus bh)
              <> encode (T._gbhExtra bh)
    decode = do
        enforceSize "GenericBlockHeader b" 5
        blockMagic <- decode
        when (blockMagic /= getProtocolMagic protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        _gbhPrevBlock <- decode
        _gbhBodyProof <- decode
        _gbhConsensus <- decode
        _gbhExtra     <- decode
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
    encode _ = encodeBytes bytes418
    decode = do
        _ :: ByteString <- decode
        let _gbHeader = error "decoded header"
            _gbBody   = error "decoded body"
            _gbExtra  = error "decoded extra"
        pure $ T.UnsafeGenericBlock {..} 

{-
    encode gb =  encodeListLen 3
              <> encode (T._gbHeader gb)
              <> encode (T._gbBody gb)
              <> encode (T._gbExtra gb)
    decode = do
        enforceSize "GenericBlock" 3
        _gbHeader <- decode
        _gbBody   <- decode
        _gbExtra  <- decode
        pure T.UnsafeGenericBlock {..}
-}

bytes418 :: ByteString
bytes418 = BS.replicate 418 0
