-- | Binary serialization of core block types.

module Pos.Binary.Core.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class   (Bi (..), label, labelS, putConst, putField)
import qualified Pos.Core.Block     as T
import qualified Pos.Binary.Cbor    as Cbor
import           Pos.Core.Constants (protocolMagic)
import qualified Pos.Core.Types     as T
import           Pos.Util.Util      (eitherToFail)

-- | This instance required only for Arbitrary instance of HeaderHash
-- due to @instance Bi a => Hash a@.
instance Bi T.BlockHeaderStub where
    size  = error "somebody tried to binary size BlockHeaderStub"
    put _ = error "somebody tried to binary put BlockHeaderStub"
    get   = error "somebody tried to binary get BlockHeaderStub"

instance Cbor.Bi T.BlockHeaderStub where
  encode = error "somebody tried to binary encode BlockHeaderStub"
  decode = fail  "somebody tried to binary decode BlockHeaderStub"

instance ( Bi (T.BHeaderHash b)
         , Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , T.BlockchainHelpers b
         ) =>
         Bi (T.GenericBlockHeader b) where
    sizeNPut = labelS "GenericBlockHeader" $
        putConst protocolMagic <>
        putField T._gbhPrevBlock <>
        putField T._gbhBodyProof <>
        putField T._gbhConsensus <>
        putField T._gbhExtra
    get = label "GenericBlockHeader" $ do
        blockMagic <- get
        when (blockMagic /= protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        prevBlock <- get
        bodyProof <- get
        consensus <- get
        extra <- get
        eitherToFail $ T.recreateGenericHeader prevBlock bodyProof consensus extra

instance ( Cbor.Bi (T.BHeaderHash b)
         , Cbor.Bi (T.BodyProof b)
         , Cbor.Bi (T.ConsensusData b)
         , Cbor.Bi (T.ExtraHeaderData b)
         , T.BlockchainHelpers b
         ) =>
         Cbor.Bi (T.GenericBlockHeader b) where
  encode bh =  Cbor.encodeListLen 5
            <> Cbor.encode protocolMagic
            <> Cbor.encode (T._gbhPrevBlock bh)
            <> Cbor.encode (T._gbhBodyProof bh)
            <> Cbor.encode (T._gbhConsensus bh)
            <> Cbor.encode (T._gbhExtra bh)
  decode = do
    Cbor.enforceSize "GenericBlockHeader b" 5
    blockMagic <- Cbor.decode
    when (blockMagic /= protocolMagic) $
        fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
    prevBlock <- Cbor.decode
    bodyProof <- Cbor.decode
    consensus <- Cbor.decode
    extra     <- Cbor.decode
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
    sizeNPut = labelS "GenericBlock" $
        putField T._gbHeader <>
        putField T._gbBody <>
        putField T._gbExtra
    get =
        label "GenericBlock" $ do
            header <- get
            body <- get
            extra <- get
            eitherToFail $ T.recreateGenericBlock header body extra

instance ( Cbor.Bi (T.BHeaderHash b)
         , Cbor.Bi (T.BodyProof b)
         , Cbor.Bi (T.ConsensusData b)
         , Cbor.Bi (T.ExtraHeaderData b)
         , Cbor.Bi (T.Body b)
         , Cbor.Bi (T.ExtraBodyData b)
         , T.BlockchainHelpers b
         ) =>
         Cbor.Bi (T.GenericBlock b) where
  encode gb =  Cbor.encodeListLen 3
            <> Cbor.encode (T._gbHeader gb)
            <> Cbor.encode (T._gbBody gb)
            <> Cbor.encode (T._gbExtra gb)
  decode = do
    Cbor.enforceSize "GenericBlock" 3
    header <- Cbor.decode
    body   <- Cbor.decode
    extra  <- Cbor.decode
    eitherToFail $ T.recreateGenericBlock header body extra
