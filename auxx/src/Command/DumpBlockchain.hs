module Command.DumpBlockchain
    ( dumpBlockchain
    ) where

import           Universum

import           Conduit (runConduit, runResourceT, sinkFile, yieldMany, (.|))
import           Control.Lens (_Wrapped)
import           Data.List.NonEmpty (last)
import           Formatting (build, sformat, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.Wlog (logInfo, logWarning)

import           Pos.Block.Dump (encodeBlockDump)
import           Pos.Core (Block, BlockHeader, EpochIndex (..), HeaderHash, blockHeaderHash,
                           difficultyL, epochIndexL, genesisHash, getBlockHeader, prevBlockL)
import           Pos.Crypto (shortHashF)
import           Pos.DB.Block (getBlock)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Error (DBError (..))
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Util.Chrono (NewestFirst (..), toOldestFirst)
import           Pos.Util.Util (maybeThrow)

import           Mode (MonadAuxxMode)

-- | Dump whole blockchain in CBOR format to the specified folder.
-- Each epoch will be at <outFolder>/epoch<epochIndex>.cbor.
dumpBlockchain
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath
    -> m ()
dumpBlockchain outFolder = withStateLock HighPriority "auxx" $ \_ -> do
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
        -- TODO: use forward links instead so that we'd be able to stream
        -- the blocks into the compression conduit
        case _Wrapped nonEmpty blocksMaybeEmpty of
            Nothing -> pass
            Just (blocks :: NewestFirst NonEmpty Block) -> do
                runResourceT $ runConduit $
                    yieldMany (toList (toOldestFirst blocks)) .|
                    encodeBlockDump .|
                    sinkFile (getOutPath epochIndex)
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
