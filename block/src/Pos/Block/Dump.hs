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
         encodeBlockDump
       , encodeBlockDump'

       -- * Decoding
       , decodeBlockDump
       , decodeBlockDump'
       , BlockDumpDecodeError (..)

       -- * Internals
       -- ** Encoding
       , encodeDumpHeader
       , encodeBlockStream
       -- ** Decoding
       , decodeDumpHeader
       , decodeBlockStream
       -- ** Stripped blocks
       , GenericBlockNoProof (..)
       , BlockNoProof
       , GenesisBlockNoProof
       , MainBlockNoProof
       , stripProof
       , addProof
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as Cbor hiding (DecodeAction (..))
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor
import           Conduit (Conduit, Consumer, Producer, runConduit, yield, (.|))
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
import           Pos.Util.Util (eitherToFail)

----------------------------------------------------------------------------
-- Encoding
----------------------------------------------------------------------------

-- | Encode a block dump. The blocks should be coming oldest-first.
encodeBlockDump
    :: (Bi BlockNoProof, Conduit.MonadResource m)
    => Conduit Block m ByteString
encodeBlockDump = (encodeDumpHeader >> encodeBlockStream)
               .| Lzma.compress Nothing

-- | Like 'encodeBlockDump', but without conduits.
encodeBlockDump'
    :: Bi BlockNoProof
    => OldestFirst [] Block -> IO LByteString
encodeBlockDump' blocks = Conduit.runResourceT $ runConduit $
    Conduit.yieldMany (toList blocks)
    .| encodeBlockDump
    .| Conduit.sinkLazy

encodeDumpHeader :: Monad m => Producer m ByteString
encodeDumpHeader = yield $ Cbor.toStrictByteString $
    Cbor.encodeListLen 2 <>                     -- tuple with 2 elements
    Cbor.encodeWord16 0 <>                      -- 1st element: version
    Cbor.encodeListLenIndef                     -- 2nd element: list

encodeBlockStream
    :: (Bi BlockNoProof, Monad m)
    => Conduit Block m ByteString
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
decodeBlockDump
    :: (Bi BlockNoProof, Conduit.MonadResource m, MonadThrow m)
    => Conduit ByteString m Block
decodeBlockDump =
    Lzma.decompress memlimit .| do
        ver <- decodeDumpHeader
        when (ver /= 0) $ throwM (UnsupportedBlockVersion ver (one 0))
        decodeBlockStream
  where
    memlimit = Just (200*1024*1024)  -- 200 MB

-- | Like 'decodeBlockDump', but without conduits.
--
-- /Throws:/ 'BlockDumpDecodeError'
decodeBlockDump'
    :: Bi BlockNoProof
    => LByteString -> IO (OldestFirst [] Block)
decodeBlockDump' dump = fmap OldestFirst $ Conduit.runResourceT $ runConduit $
    Conduit.sourceLazy dump
    .| decodeBlockDump
    .| Conduit.sinkList

-- | Deserialize the header of a dump and return data decoded from the
-- header (i.e. block version).
decodeDumpHeader
    :: (MonadThrow m, MonadIO m)
    => Consumer ByteString m Word16
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
    => Conduit ByteString m Block
decodeBlockStream = go 0
  where
    go :: Bi BlockNoProof => Int -> Conduit ByteString m Block
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
         , C.BlockchainHelpers b
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
        Bi.enforceSize "GenericBlock" 3
        (prevBlock, consensus, headerExtra) <- decodeHeader
        body  <- decode
        extra <- decode
        let bodyProof = C.mkBodyProof body
        header <- eitherToFail $
            C.recreateGenericHeader prevBlock bodyProof consensus headerExtra
        block <- eitherToFail $ C.recreateGenericBlock header body extra
        pure $ GenericBlockNoProof block
      where
        decodeHeader = do
            Bi.enforceSize "GenericBlockHeader b" 4
            blockMagic <- decode
            when (blockMagic /= C.protocolMagic) $
                fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
            prevBlock <- decode
            consensus <- decode
            extra     <- decode
            pure (prevBlock, consensus, extra)
