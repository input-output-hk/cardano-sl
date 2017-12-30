module Command.Dump
    ( dump
    ) where

import           Universum

import           Codec.Compression.Lzma (compress)
import           Control.Lens (_Wrapped)
import qualified Data.ByteString.Lazy as BSL
import           Data.List.NonEmpty (last)
import           Formatting (build, sformat, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.Wlog (logInfo, logWarning)

import           Pos.Binary (Bi (..), encodeListLen, enforceSize, serialize)
import           Pos.Core (Block, BlockHeader, EpochIndex (..), HeaderHash, blockHeaderHash,
                           difficultyL, epochIndexL, genesisHash, getBlockHeader, prevBlockL)
import qualified Pos.Core as C
import           Pos.Core.Block (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Crypto (HasCryptoConfiguration, getProtocolMagic, protocolMagic, shortHashF)
import           Pos.DB.Block (getBlock)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Error (DBError (..))
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (eitherToFail, maybeThrow)

import           Mode (MonadAuxxMode)

newtype GenericBlockNoProof b = GenericBlockNoProof (GenericBlock b)

type BlockNoProof = Either GenesisBlockNoProof MainBlockNoProof
type GenesisBlockNoProof = GenericBlockNoProof C.GenesisBlockchain
type MainBlockNoProof = GenericBlockNoProof C.MainBlockchain

instance ( Typeable b
         , Bi (C.BHeaderHash b)
         , Bi (C.ConsensusData b)
         , Bi (C.ExtraHeaderData b)
         , Bi (C.Body b)
         , Bi (C.ExtraBodyData b)
         , C.BlockchainHelpers b
         , HasCryptoConfiguration
         ) =>
         Bi (GenericBlockNoProof b) where
    encode (GenericBlockNoProof gb) =
        encodeListLen 3
        <> encodeHeader (_gbHeader gb)
        <> encode (_gbBody gb)
        <> encode (_gbExtra gb)
      where
        encodeHeader bh =
            encodeListLen 4
            <> encode (getProtocolMagic protocolMagic)
            <> encode (_gbhPrevBlock bh)
            <> encode (_gbhConsensus bh)
            <> encode (_gbhExtra bh)

    decode = do
        enforceSize "GenericBlock" 3
        (prevBlock, consensus, headerExtra) <- decodeHeader'
        body  <- decode
        extra <- decode
        let bodyProof = C.mkBodyProof body
        header <- eitherToFail $
            C.recreateGenericHeader prevBlock bodyProof consensus headerExtra
        block <- eitherToFail $ C.recreateGenericBlock header body extra
        pure $ GenericBlockNoProof block
      where
        decodeHeader' = do
            enforceSize "GenericBlockHeader b" 4
            blockMagic <- decode
            when (blockMagic /= getProtocolMagic protocolMagic) $
                fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
            prevBlock <- decode
            consensus <- decode
            extra     <- decode
            pure $ (prevBlock, consensus, extra)

stripProof :: Block -> BlockNoProof
stripProof (Left gb)  = Left $ GenericBlockNoProof gb
stripProof (Right mb) = Right $ GenericBlockNoProof mb

-- | Dump whole blockchain in CBOR format to the specified folder.
-- Each epoch will be at <outFolder>/epoch<epochIndex>.cbor.
dump
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath
    -> m ()
dump outFolder = withStateLock HighPriority "auxx" $ \_ -> do
    printTipDifficulty
    liftIO $ createDirectoryIfMissing True outFolder
    tipHeader <- DB.getTipHeader
    doDump tipHeader
  where
    printTipDifficulty :: MonadAuxxMode m => m ()
    printTipDifficulty = do
        tipDifficulty <- view difficultyL <$> DB.getTipHeader
        logInfo $ sformat ("Our tip's difficulty is "%build) tipDifficulty

    loadEpoch
        :: MonadAuxxMode m
        => HeaderHash
        -> m (Maybe HeaderHash, NewestFirst [] Block)
    loadEpoch start = do
        -- returns Maybe (tip of previous epoch)
        (maybePrev, blocksMaybeEmpty) <- doIt [] start
        pure (maybePrev, NewestFirst blocksMaybeEmpty)
      where
        doIt :: MonadAuxxMode m => [Block] -> HeaderHash -> m (Maybe HeaderHash, [Block])
        doIt !acc h = do
            d <- getBlockThrow h
            let newAcc = d : acc
                prev = d ^. prevBlockL
            case d of
                Left _ -> pure
                    ( if prev == genesisHash then Nothing else Just prev
                    , reverse newAcc)
                Right _ ->
                    doIt newAcc prev

    doDump
        :: (MonadAuxxMode m, MonadIO m)
        => BlockHeader
        -> m ()
    doDump start = do
        let epochIndex = start ^. epochIndexL
        logInfo $ sformat ("Processing "%build) epochIndex
        (maybePrev, blocksMaybeEmpty) <- loadEpoch $ blockHeaderHash start
        case _Wrapped nonEmpty blocksMaybeEmpty of
            Nothing -> pass
            Just (blocks :: NewestFirst NonEmpty Block) -> do
                let stripped   = map stripProof blocks
                    serialized = serialize (0 :: Word8, stripped)
                    outPath    = getOutPath epochIndex
                liftIO $ BSL.writeFile outPath $ compress serialized
                whenJust maybePrev $ \prev -> do
                    maybeHeader <- DB.getHeader prev
                    case maybeHeader of
                        Just header -> doDump header
                        Nothing     ->
                            let anchorHash =
                                    blocks &
                                    getNewestFirst &
                                    last &
                                    getBlockHeader &
                                    blockHeaderHash in
                            logWarning $ sformat ("DB contains block "%build%
                                " but not its parent "%build) anchorHash prev

    getOutPath :: EpochIndex -> FilePath
    getOutPath epochIndex =
        let outFile = toString $
                sformat ("epoch"%build%".cbor.lzma") (getEpochIndex epochIndex)
        in outFolder </> outFile

    getBlockThrow
        :: MonadDBRead m
        => HeaderHash -> m Block
    getBlockThrow hash =
        maybeThrow (DBMalformed $ sformat errFmt hash) =<< getBlock hash
      where
        errFmt = "getBlockThrow: no block with HeaderHash: "%shortHashF
