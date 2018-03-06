{-# LANGUAGE RankNTypes #-}

{- | Block dumps (used for snapshots).

A block dump looks like this:

@
[block version, [block1, block2, ...]]
@

In CBOR terms, this is translated into this sequence of tokens:

@
listlen=2, block version, list_indef_len, [block1] [block2] ...
@

When we encode blocks, we strip proofs from them. When we decode blocks, we
reconstruct those proofs.

-}

module Pos.Block.Dump
       (
       -- * Encoding
         encodeBlockDumpC
       , encodeBlockDump

       -- * Decoding
       , decodeBlockDumpC
       , decodeBlockDump
       , BlockDumpDecodeError (..)

       -- * Internals
       -- ** Encoding
       , encodeDumpHeader
       , encodeBlockStream
       -- ** Decoding
       , decodeDumpHeader
       , decodeBlockStream
       -- ** Stripped blocks
       , BlockNoProof
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as Cbor hiding (DecodeAction (..))
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor
import           Conduit (ConduitT, runConduit, yield, (.|))
import qualified Conduit
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.Text.Buildable
import           Formatting (bprint, int, stext, (%))
import           Serokell.Util (listJson)

import           Pos.Binary.Class (Bi (..), serialize')
import qualified Pos.Binary.Class as Bi
import qualified Pos.Binary.Conduit as Bi
import           Pos.Core (Block, GenericBlock (..), GenericBlockHeader (..))
import qualified Pos.Core as C
import           Pos.Crypto (HasCryptoConfiguration)
import           Pos.Util.Chrono (OldestFirst (..))

----------------------------------------------------------------------------
-- Encoding
----------------------------------------------------------------------------

-- | Encode a block dump. The blocks should be coming oldest-first.
encodeBlockDumpC
    :: (Bi BlockNoProof, Conduit.MonadResource m)
    => ConduitT Block ByteString m ()
encodeBlockDumpC = (encodeDumpHeader >> encodeBlockStream)
               .| Lzma.compress Nothing

-- | Like 'encodeBlockDumpC', but without conduits.
encodeBlockDump
    :: Bi BlockNoProof
    => OldestFirst [] Block -> IO LByteString
encodeBlockDump blocks = Conduit.runResourceT $ runConduit $
    Conduit.yieldMany (toList blocks)
    .| encodeBlockDumpC
    .| Conduit.sinkLazy

encodeDumpHeader
    :: forall i m . (Monad m)
    => ConduitT i ByteString m ()
encodeDumpHeader = yield $ Cbor.toStrictByteString $
    Cbor.encodeListLen 2 <>                     -- tuple with 2 elements
    Cbor.encodeWord16 0 <>                      -- 1st element: version
    Cbor.encodeListLenIndef                     -- 2nd element: list

encodeBlockStream
    :: (Bi BlockNoProof, Monad m)
    => ConduitT Block ByteString m ()
encodeBlockStream = Conduit.mapC (serialize' . stripProof)

----------------------------------------------------------------------------
-- Decoding
----------------------------------------------------------------------------

-- | All errors that can happen when decoding a block dump.
data BlockDumpDecodeError

    -- | Couldn't decode a block
    = BlockDecodeError
        { bddeBlockIndex :: !Int            -- ^ at which block we failed
        , bddeByteOffset :: !Int64          -- ^ offset inside the block
        , bddeMessage    :: !Text }

    -- | The dump (header) has wrong structure
    | DumpFormatError
        { bddeByteOffset :: !Int64
        , bddeMessage    :: !Text }

    -- | The dump is for “bad” block version which we don't support
    | UnsupportedBlockVersion
        { bddeFoundVersion    :: !Word16
        , bddeExpectedVersion :: !(Set Word16) }

    -- | The dump is completely empty
    | EmptyInput

    deriving (Show, Eq)

instance Exception BlockDumpDecodeError

instance Buildable BlockDumpDecodeError where
    build (BlockDecodeError index offset reason) =
        bprint ("Failed to decode block with index "%int%": "%
                "at offset "%int%": "%stext) index offset reason
    build (DumpFormatError offset reason) =
        bprint ("Block dump has wrong format: "%
                "at offset "%int%": "%stext) offset reason
    build (UnsupportedBlockVersion got expected) =
        bprint ("Expected one of block versions "%listJson%", got "%int)
               expected got
    build EmptyInput =
        bprint "Block dump is completely empty (no bytes)"

-- | A conduit that decodes blocks from a CBOR-encoded LZMA-compressed list.
--
-- /Throws:/ 'BlockDumpDecodeError'
decodeBlockDumpC
    :: (Bi BlockNoProof, Conduit.MonadResource m, MonadThrow m)
    => ConduitT ByteString Block m ()
decodeBlockDumpC =
    Lzma.decompress memlimit .| do
        ver <- decodeDumpHeader
        when (ver /= 0) $ throwM (UnsupportedBlockVersion ver (one 0))
        decodeBlockStream
  where
    memlimit = Just (200*1024*1024)  -- 200 MB

-- | Like 'decodeBlockDumpC', but without conduits.
--
-- /Throws:/ 'BlockDumpDecodeError'
decodeBlockDump
    :: Bi BlockNoProof
    => LByteString -> IO (OldestFirst [] Block)
decodeBlockDump dump = fmap OldestFirst $ Conduit.runResourceT $ runConduit $
    Conduit.sourceLazy dump
    .| decodeBlockDumpC
    .| Conduit.sinkList

-- | Deserialize the header of a dump and return data decoded from the
-- header (i.e. block version).
decodeDumpHeader
    :: forall o m . (MonadThrow m, MonadIO m)
    => ConduitT ByteString o m Word16
decodeDumpHeader =
    Bi.awaitCbor decoder >>= \case
        Nothing ->
            throwM EmptyInput
        Just (Left (Cbor.DeserialiseFailure off err)) ->
            throwM (DumpFormatError off (toText err))
        Just (Right x) ->
            pure x
  where
    -- A decoder for everything preceding the block stream
    decoder = do
        Cbor.decodeListLenCanonicalOf 2        -- tuple with two elements
        ver <- Cbor.decodeWord16Canonical      -- 1st element: version
        Cbor.decodeListLenIndef                -- 2nd element: list
        pure ver

-- | Decode a stream of blocks.
decodeBlockStream
    :: forall m. (Bi BlockNoProof, MonadIO m, MonadThrow m)
    => ConduitT ByteString Block m ()
decodeBlockStream = go 0
  where
    go :: Bi BlockNoProof => Int -> ConduitT ByteString Block m ()
    go i = Bi.awaitBi >>= \case
        Nothing ->
            pure ()
        Just (Left (Cbor.DeserialiseFailure off err)) ->
            throwM (BlockDecodeError i off (toText err))
        Just (Right block) -> do
            yield (addProof block)
            go (i + 1)

----------------------------------------------------------------------------
-- Stripped blocks
----------------------------------------------------------------------------

-- | To make snapshots more compact, we strip proofs from blocks. This is a
-- helper newtype for that.
newtype GenericBlockNoProof b = GenericBlockNoProof (GenericBlock b)

type GenesisBlockNoProof = GenericBlockNoProof C.GenesisBlockchain
type MainBlockNoProof    = GenericBlockNoProof C.MainBlockchain

type BlockNoProof = Either GenesisBlockNoProof MainBlockNoProof

stripProof :: Block -> BlockNoProof
stripProof (Left gb)  = Left  $ GenericBlockNoProof gb
stripProof (Right mb) = Right $ GenericBlockNoProof mb

addProof :: BlockNoProof -> Block
addProof (Left  (GenericBlockNoProof gb)) = Left gb
addProof (Right (GenericBlockNoProof mb)) = Right mb

-- | Encode the block without including any proofs. When decoding, recreate
-- the proofs.
instance ( Typeable b
         , Bi (C.BHeaderHash b)
         , Bi (C.ConsensusData b)
         , Bi (C.ExtraHeaderData b)
         , Bi (C.Body b)
         , Bi (C.ExtraBodyData b)
         , C.Blockchain b
         , HasCryptoConfiguration
         , C.HasProtocolConstants
         ) =>
         Bi (GenericBlockNoProof b) where
    encode (GenericBlockNoProof gb) =
        Bi.encodeListLen 3
        <> encodeHeader (_gbHeader gb)
        <> encode (_gbBody gb)
        <> encode (_gbExtra gb)
      where
        encodeHeader bh =
            Bi.encodeListLen 4
            <> encode C.protocolMagic
            <> encode (_gbhPrevBlock bh)
            <> encode (_gbhConsensus bh)
            <> encode (_gbhExtra bh)

    decode = do
        Bi.enforceSize "GenericBlockNoProof" 3
        (prevBlock, consensus, headerExtra) <- decodeHeader
        body  <- decode
        extra <- decode
        let bodyProof = C.mkBodyProof body
            header = C.UnsafeGenericBlockHeader prevBlock bodyProof consensus headerExtra
            block = C.UnsafeGenericBlock header body extra
        pure $ GenericBlockNoProof block
      where
        decodeHeader = do
            Bi.enforceSize "GenericBlockHeader (proofless)" 4
            blockMagic <- decode
            when (blockMagic /= C.protocolMagic) $
                Bi.cborError $ "GenericBlockHeader (proofless) failed " <>
                               "with wrong magic: " <> show blockMagic
            prevBlock <- decode
            consensus <- decode
            extra     <- decode
            pure (prevBlock, consensus, extra)
