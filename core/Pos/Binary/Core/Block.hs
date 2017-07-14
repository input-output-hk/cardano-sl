-- | Binary serialization of core block types.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), encodeListLen, enforceSize)
import qualified Pos.Core.Block     as T
import           Pos.Core.Constants (protocolMagic)
import qualified Pos.Core.Types     as T
import           Pos.Util.Util      (eitherToFail)

-- | This instance required only for Arbitrary instance of HeaderHash
-- due to @instance Bi a => Hash a@.
instance Bi T.BlockHeaderStub where
  encode = error "somebody tried to binary encode BlockHeaderStub"
  decode = fail  "somebody tried to binary decode BlockHeaderStub"

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , T.BlockchainHelpers b
         ) =>
         Bi (T.GenericBlockHeader b) where
  encode bh =  encodeListLen 5
            <> encode protocolMagic
            <> encode (T._gbhPrevBlock bh)
            <> encode (T._gbhBodyProof bh)
            <> encode (T._gbhConsensus bh)
            <> encode (T._gbhExtra bh)
  decode = do
    enforceSize "GenericBlockHeader b" 5
    blockMagic <- decode
    when (blockMagic /= protocolMagic) $
        fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
    prevBlock <- decode
    bodyProof <- decode
    consensus <- decode
    extra     <- decode
    eitherToFail $ T.recreateGenericHeader prevBlock bodyProof consensus extra

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         , T.BlockchainHelpers b
         ) =>
         Bi (T.GenericBlock b) where
  encode gb =  encodeListLen 3
            <> encode (T._gbHeader gb)
            <> encode (T._gbBody gb)
            <> encode (T._gbExtra gb)
  decode = do
    enforceSize "GenericBlock" 3
    header <- decode
    body   <- decode
    extra  <- decode
    eitherToFail $ T.recreateGenericBlock header body extra
