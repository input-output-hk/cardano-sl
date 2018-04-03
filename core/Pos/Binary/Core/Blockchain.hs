-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Universum

import           Pos.Binary.Class (Bi (..), decodeListLenCanonicalOf, encodeListLen, enforceSize)
import           Pos.Binary.Core.Block ()
import           Pos.Binary.Core.Common ()
import qualified Pos.Core.Block.Blockchain as T
import           Pos.Core.Block.Union.Types (BlockHeader (..))
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Util.Util (cborError)

instance ( Typeable b
         , Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         ) =>
         Bi (T.GenericBlockHeader b) where
    encode bh =  encodeListLen 5
              <> encode (getProtocolMagic (T._gbhProtocolMagic bh))
              <> encode (T._gbhPrevBlock bh)
              <> encode (T._gbhBodyProof bh)
              <> encode (T._gbhConsensus bh)
              <> encode (T._gbhExtra bh)
    decode = do
        enforceSize "GenericBlockHeader b" 5
        _gbhProtocolMagic <- ProtocolMagic <$> decode
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
         ) =>
         Bi (T.GenericBlock b) where
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

----------------------------------------------------------------------------
-- BlockHeader
----------------------------------------------------------------------------

instance Bi BlockHeader where
   encode x = encodeListLen 2 <> encodeWord tag <> body
     where
       (tag, body) = case x of
         BlockHeaderGenesis bh -> (0, encode bh)
         BlockHeaderMain bh    -> (1, encode bh)

   decode = do
       decodeListLenCanonicalOf 2
       t <- decodeWordCanonical
       case t of
           0 -> BlockHeaderGenesis <$!> decode
           1 -> BlockHeaderMain <$!> decode
           _ -> cborError $ "decode@BlockHeader: unknown tag " <> pretty t
