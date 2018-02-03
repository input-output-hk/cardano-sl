module Command.DumpBlockchain
    ( dumpBlockchain
    , applyBlockchainDump
    ) where

import           Universum

import qualified Algorithms.NaturalSort as NaturalSort
import           Conduit (runConduitRes, (.|))
import qualified Conduit as C
import           Control.Lens (_Wrapped)
import           Data.List.NonEmpty (last)
import           Fmt ((+|), (|+))
import           Formatting (build, sformat, (%))
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))
import           System.Wlog (logInfo, logWarning)

import           Pos.Block.Dump (decodeBlockDumpC, encodeBlockDumpC)
import           Pos.Block.Logic.VAR (verifyAndApplyBlocksC)
import           Pos.Core (Block, BlockHeader, EpochIndex (..), HeaderHash, difficultyL,
                           epochIndexL, genesisHash, getBlockHeader, headerHash, prevBlockL)
import           Pos.Crypto (shortHashF)
import           Pos.DB.Block (getBlock)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Error (DBError (..))
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Util.Chrono (NewestFirst (..), toOldestFirst)
import           Pos.Util.Util (maybeThrow)

import           Mode (MonadAuxxMode)

----------------------------------------------------------------------------
-- Creating a dump
----------------------------------------------------------------------------

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
        (maybePrev, blocksMaybeEmpty) <- loadEpoch $ headerHash start
        -- TODO: use forward links instead so that we'd be able to stream
        -- the blocks into the compression conduit
        case _Wrapped nonEmpty blocksMaybeEmpty of
            Nothing -> pass
            Just (blocks :: NewestFirst NonEmpty Block) -> do
                runConduitRes $
                    C.yieldMany (toList (toOldestFirst blocks)) .|
                    encodeBlockDumpC .|
                    C.sinkFile (getOutPath epochIndex)
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
                                    headerHash in
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

----------------------------------------------------------------------------
-- Loading a dump
----------------------------------------------------------------------------

-- | Load blocks from a file or directory and apply them.
applyBlockchainDump
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath -> m ()
applyBlockchainDump path = withStateLock HighPriority "auxx" $ \_ -> do
    isDir <- liftIO $ doesDirectoryExist path
    if isDir then applyDir path else applyEpoch path

-- | Apply all epochs from a directory.
applyDir
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath -> m ()
applyDir path = do
    files <- sortBy NaturalSort.compare <$> liftIO (listDirectory path)
    for_ files $ \f -> applyEpoch (path </> f)

-- | Apply a single epoch stored in a file.
applyEpoch
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath -> m ()
applyEpoch path = do
    (tip, difficulty) <- getTipAndDifficulty

    logInfo ("Applying blocks from "+|path|+", tip is "+|tip|+", " <>
             "difficulty is "+|difficulty|+"")
    result <- runConduitRes $
           C.sourceFile path
        .| decodeBlockDumpC                            -- get a stream of blocks
        .| (C.dropWhileC ((/= tip) . view prevBlockL) -- skip blocks until tip
        >> verifyAndApplyBlocksC True)                -- apply blocks w/ rollback
    whenLeft result throwM

    (newTip, newDifficulty) <- getTipAndDifficulty
    if tip == newTip
        then logInfo ("All blocks were skipped because they can't " <>
                      "be applied to the current tip")
        else logInfo ("Applied blocks: "+|newDifficulty - difficulty|+". " <>
                      "The current tip is "+|newTip|+"")
  where
    getTipAndDifficulty = (headerHash &&& view difficultyL) <$> DB.getTipHeader
