module Command.DumpBlockchain
    ( dumpBlockchain
    , dumpBlockchainLoop
    , applyBlockchainDump
    ) where

import           Universum

import           Conduit (runConduitRes, (.|))
import qualified Conduit as C
import           Control.Lens (_Wrapped)
import           Data.List.NonEmpty (last)
import           Fmt (format, (+|), (|+))
import           Formatting (build, sformat, (%))
import           Mockable (delay)
import           Safe (maximumMay)
import           Serokell.Util (hour)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import           System.FilePath ((</>))
import           System.Wlog (logInfo, logWarning)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MP

import           Pos.Block.Dump (decodeBlockDumpC, encodeBlockDumpC)
import           Pos.Block.Logic.VAR (verifyAndApplyBlocksC)
import           Pos.Core (Block, BlockHeader (..), EpochIndex (..), HeaderHash, difficultyL,
                           epochIndexL, flattenEpochOrSlot, genesisHash, getEpochOrSlot,
                           getSlotCount, headerHash, prevBlockL, slotSecurityParam, unflattenSlotId)
import           Pos.DB.Block.Load (getBlockThrow, getHeaderThrow)
import qualified Pos.DB.BlockIndex as DB
import           Pos.StateLock (Priority (..), withStateLock)
import           Pos.Util.Chrono (NewestFirst (..), toOldestFirst)
import           Pos.Util.Util (subtractMay)

import           Mode (MonadAuxxMode)

----------------------------------------------------------------------------
-- Creating a dump
----------------------------------------------------------------------------

-- | Dump whole blockchain in CBOR format to the specified folder.
-- Each epoch will be at <outFolder>/epoch<epochIndex>.cbor.
dumpBlockchain
    :: (MonadAuxxMode m, MonadIO m)
    => Maybe EpochIndex                 -- ^ Start epoch (Nothing = 0)
    -> Maybe EpochIndex                 -- ^ End epoch (Nothing = last block)
    -> FilePath
    -> m ()
dumpBlockchain startEpoch endEpoch outFolder =
  withStateLock HighPriority "auxx" $ \_ -> do
    logInfo $ format "Going to dump epochs from {} up to {}"
                     (maybe "the beginning" pretty startEpoch)
                     (maybe "the last epoch" pretty endEpoch)
    printTipDifficulty
    liftIO $ createDirectoryIfMissing True outFolder
    tipHeader <- DB.getTipHeader
    doDump tipHeader
    logInfo "Finished dumping epochs"
  where
    startEpoch' = fromMaybe 0 startEpoch
    endEpoch'   = fromMaybe maxBound endEpoch

    printTipDifficulty :: MonadAuxxMode m => m ()
    printTipDifficulty = do
        tipDifficulty <- view difficultyL <$> DB.getTipHeader
        logInfo $ sformat ("Our tip's difficulty is "%build) tipDifficulty

    doDump
        :: (MonadAuxxMode m, MonadIO m)
        => BlockHeader
        -> m ()
    doDump start = do
        let epochIndex = start ^. epochIndexL
        if | epochIndex > endEpoch' -> do         -- we're not in the range yet
                 next <- fastForwardEpoch start
                 whenJust next doDump
           | epochIndex >= startEpoch' -> do      -- we're in the range
                 next <- dumpEpoch start
                 whenJust next doDump
           | otherwise -> pure ()                 -- we're outside of the range

    dumpEpoch
        :: (MonadAuxxMode m, MonadIO m)
        => BlockHeader -> m (Maybe BlockHeader)
    dumpEpoch start = do
        let epochIndex = start ^. epochIndexL
            outPath    = outFolder </> epochFileName epochIndex
        logInfo ("Dumping epoch "+|epochIndex|+" into "+|outPath|+"")
        (maybePrev, blocksMaybeEmpty) <- loadEpoch $ headerHash start
        -- TODO: use forward links instead so that we'd be able to stream
        -- the blocks into the compression conduit
        case _Wrapped nonEmpty blocksMaybeEmpty of
            Nothing -> pure Nothing
            Just (blocks :: NewestFirst NonEmpty Block) -> do
                runConduitRes $
                    C.yieldMany (toList (toOldestFirst blocks)) .|
                    encodeBlockDumpC .|
                    C.sinkFile outPath
                case maybePrev of
                    Nothing -> pure Nothing
                    Just prev -> DB.getHeader prev >>= \case
                        Just header -> pure (Just header)
                        Nothing     -> do
                            let anchor = headerHash $
                                         last (getNewestFirst blocks)
                            logWarning ("DB contains block "+|anchor|+
                                        " but not its parent "+|prev|+"")
                            pure Nothing

    fastForwardEpoch
        :: (MonadAuxxMode m, MonadIO m)
        => BlockHeader -> m (Maybe BlockHeader)
    fastForwardEpoch start = do
        let epochIndex = start ^. epochIndexL
        logInfo ("Skipping epoch "+|epochIndex|+"")
        maybePrev <- skipEpoch $ headerHash start
        case maybePrev of
            Nothing -> pure Nothing
            Just prev -> DB.getHeader prev >>= \case
                Just header -> pure (Just header)
                Nothing     -> do
                    logWarning ("DB doesn't contain block "+|prev|+"")
                    pure Nothing

-- | Run an endless loop that will be dumping epochs as soon as they become
-- stable.
dumpBlockchainLoop
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath
    -> m ()
dumpBlockchainLoop outFolder = forever $ do
    -- Get the last stable epoch – i.e. one that won't ever change or be
    -- rolled back. To do that, we subtract 2K+2 slots from the last block's
    -- slot and take the *previous* epoch (2K+2 instead of 2K+1 because the
    -- last block might be a genesis block and then it would correspond to
    -- slot N:0 even though the block N:0 wasn't issued yet).
    lastSlot <- flattenEpochOrSlot . getEpochOrSlot <$> DB.getTipHeader
    let mbStableEpoch = do
            stableSlot <- subtractMay (getSlotCount (slotSecurityParam + 2))
                                      lastSlot
            subtractMay 1 (unflattenSlotId stableSlot ^. epochIndexL)
    -- Look at the filenames of existing files to determine the last epoch
    -- which we have already dumped.
    mbLastDumpedEpoch <-
        maximumMay . mapMaybe parseEpochFileName <$>
        liftIO (listDirectory outFolder)
    -- Dump all epochs in the interval [last dumped + 1; stable], or all
    -- epochs until 'stable' if there are no dumped epochs.
    case (mbStableEpoch, mbLastDumpedEpoch) of
        (Nothing, _) ->
            logInfo "No epoch is stable yet; not going to dump anything"
        (Just stableEpoch, Nothing) -> do
            logInfo ("The last stable epoch is "+|stableEpoch|+"")
            dumpBlockchain Nothing (Just stableEpoch) outFolder
        (Just stableEpoch, Just lastDumpedEpoch)
            | stableEpoch <= lastDumpedEpoch ->
                  logInfo ("Not going to dump anything; the last " <>
                           "stable epoch ("+|stableEpoch|+") is " <>
                           "already dumped")
            | otherwise -> do
                  logInfo ("The last stable epoch is "+|stableEpoch|+"")
                  dumpBlockchain (Just (lastDumpedEpoch + 1))
                                 (Just stableEpoch)
                                 outFolder
    -- Wait before repeating. Without the delay, we'll spend all the time
    -- doing IO. (A one-hour delay before a fresh epoch becomes available
    -- seems fine, given that epochs last several days.)
    delay (hour 1)

-- | Get all blocks from epoch boundary up to the given block.
--
-- Returns blocks and the previous epoch's tip (or 'Nothing' if it was the
-- first epoch).
loadEpoch
    :: MonadAuxxMode m
    => HeaderHash -> m (Maybe HeaderHash, NewestFirst [] Block)
loadEpoch start = do
    (maybePrev, blocksMaybeEmpty) <- doIt [] start
    pure (maybePrev, NewestFirst blocksMaybeEmpty)
  where
    doIt :: MonadAuxxMode m
         => [Block] -> HeaderHash -> m (Maybe HeaderHash, [Block])
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

-- | Skip blocks up to and including the epoch boundary (i.e. it's like
-- 'loadEpoch', but doesn't load the blocks from the database).
--
-- Returns the previous epoch's tip (or 'Nothing' if it was the first
-- epoch).
skipEpoch
    :: MonadAuxxMode m
    => HeaderHash -> m (Maybe HeaderHash)
skipEpoch start = do
    header <- getHeaderThrow start
    let prev = header ^. prevBlockL
    case header of
        BlockHeaderGenesis _ ->
            pure (if prev == genesisHash then Nothing else Just prev)
        BlockHeaderMain _ ->
            skipEpoch prev

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
    files <- sortOn parseEpochFileName .
             filter (isJust . parseEpochFileName)
               <$> liftIO (listDirectory path)
    for_ files $ \f -> applyEpoch (path </> f)

-- | Apply a single epoch stored in a file.
applyEpoch
    :: (MonadAuxxMode m, MonadIO m)
    => FilePath -> m ()
applyEpoch path = do
    (tip, difficulty) <- getTipAndDifficulty
    logInfo ("Applying blocks from "+|path|+", tip is "+|tip|+", " <>
             "difficulty is "+|difficulty|+"")
    (mbErr, _) <- runConduitRes
           -- get a stream of blocks
         $ C.sourceFile path .| decodeBlockDumpC
        .| do -- skip all blocks that we have (i.e. up to and including the tip)
              C.dropWhileC ((/= tip) . view prevBlockL)
              -- apply the rest of the blocks, with rollback
              C.transPipe lift (verifyAndApplyBlocksC True)
    (newTip, newDifficulty) <- getTipAndDifficulty
    if tip == newTip
        then logInfo ("All blocks were skipped because they can't " <>
                      "be applied to the current tip")
        else logInfo ("Applied blocks: "+|newDifficulty - difficulty|+". " <>
                      "The current tip is "+|newTip|+"")
    whenJust mbErr throwM
  where
    getTipAndDifficulty = (headerHash &&& view difficultyL) <$> DB.getTipHeader

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

epochFileName :: EpochIndex -> FilePath
epochFileName (EpochIndex n) = "epoch"+|n|+".cbor.lzma"

parseEpochFileName :: FilePath -> Maybe EpochIndex
parseEpochFileName = MP.parseMaybe @Void $
    MP.between (MP.string "epoch") (MP.string ".cbor.lzma") MP.decimal
