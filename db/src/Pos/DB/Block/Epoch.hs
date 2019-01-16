{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module contains the code for consolidating blund (block/undo) files
-- into an epoch/index file pair.
--
-- There are up to 21600 blocks per epoch and these are consolidated into an
-- epoch file and an index for lookup the blund by 'SlotId'. Once an entire
-- epoch has been consolidated into an epoch/index file pair, the old blund
-- files are deleted.
--
-- Epoch files (which have a header containing version information) consist
-- of the concatenation of the data for all the slots within the epoch, where
-- the data for a slot is stored as as the concatenation of:
--   * the four characters "blnd" to mark the start of a slot (for debugging)
--   * a 32 bit value for the length of the block
--   * a 32 bit value for the length of the undo
--   * the block itself
--   * the undo itself
--
-- Epoch index files contain a header and then an indexed vector of the offset
-- into the file where the data for a slot will be found. If an offset in the
-- file has all its bits set, then that mean that there is no block/undo for
-- that slot index.
--
-- The consolidation process always leaves at least the last two epochs
-- unconsolidated so that this code can ignore the possibility of rollbacks.

-- Implementation details below.

module Pos.DB.Block.Epoch
       ( consolidateWorker

       , dbGetConsolidatedSerBlundRealDefault
       , dbGetConsolidatedSerBlockRealDefault
       , dbGetConsolidatedSerUndoRealDefault
       ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe (SomeException, handle)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Binary (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (partitionEithers)
import           Data.List.Extra (chunksOf)
import           Formatting (build, int, sformat, shown, (%))
import           System.Directory (removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (..), SeekMode (..), hClose, hSeek,
                     openBinaryFile, withBinaryFile)
import           System.IO.Error (isDoesNotExistError)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Block (HeaderHash, blockHeaderHash)
import           Pos.Chain.Genesis as Genesis (Config (..), GenesisHash,
                     configEpochSlots)
import           Pos.Core (EpochIndex (..), EpochOrSlot (..),
                     LocalSlotIndex (..), SlotCount (..), SlotId (..),
                     getEpochOrSlot)
import           Pos.DB.Block.GState.BlockExtra (getFirstGenesisBlockHash,
                     resolveForwardLink)
import           Pos.DB.Block.Internal (bspBlund, dbGetSerBlockRealFile,
                     dbGetSerBlundRealFile, dbGetSerUndoRealFile, getAllPaths,
                     getSerializedBlund)
import           Pos.DB.BlockIndex (getHeader, getTipHeader)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), Serialized (..),
                     SerializedBlock, SerializedBlund, SerializedUndo)
import           Pos.DB.Epoch.Index (SlotIndexOffset (..), getEpochBlundOffset,
                     writeEpochIndex)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.DB.Rocks.Types (MonadRealDB, blockDataDir, epochDataDir,
                     epochLock, getNodeDBs)
import           Pos.Util.Concurrent.RWLock (whenAcquireWrite)
import           Pos.Util.Wlog (CanLog, HasLoggerName, logError, logInfo,
                     usingLoggerName)

-- | Block/epoch consolidation worker.
-- This function is only ever called once from 'bracketNodeResources' and is
-- started as an `'Async' process and does not terminate until the 'Async' is
-- cancelled when 'bracketNodeResources' releases the 'NodeResources'.
consolidateWorker
    :: (CanLog m, MonadCatch m, MonadDB m, MonadMask m, MonadRealDB ctx m)
    => Genesis.Config -> m ()
consolidateWorker genesisConfig =
    usingLoggerName "consolidate" $
        handle handler $ do
            checkPoint <- getConsolidateCheckPoint genesisHash
            tipEpoch <- getTipEpoch
            logInfo $ sformat ("current tip epoch is "%int%", "%int%" epochs consolidated")
                        (getEpochIndex tipEpoch) (getEpochIndex $ ccpEpochIndex checkPoint)
            forever $ do
                -- Don't start consolidation until the node has been running for
                -- bit. Want the node to do all its initialisation, but do not want
                -- it to sync too many blocks before starting consolidation.
                sleepSeconds 15
                loop $ CSSyncSeconds 2
  where
    epochSlots :: SlotCount
    epochSlots = configEpochSlots genesisConfig

    genesisHash = configGenesisHash genesisConfig

    -- Since this module uses 'Control.Exception.Safe' catching 'SomeException'
    -- will only catch synchronous exceptions, which we just log.
    -- Sleep for 15 seconds after logging to mitigate problems with an exception
    -- being continuously rethrown.
    handler :: (CanLog m, HasLoggerName m, MonadIO m) => SomeException -> m ()
    handler e = do
        logError $ sformat shown e
        sleepSeconds 15

    loop
        :: (HasLoggerName m, CanLog m, MonadDB m, MonadMask m, MonadRealDB ctx m)
        => ConsolidateStatus -> m ()
    loop !oldCs = do
        elock <- view epochLock <$> getNodeDBs
        mcs <- whenAcquireWrite elock $ do
                checkPoint <- getConsolidateCheckPoint genesisHash
                tipEpoch <- getTipEpoch
                if ccpEpochIndex checkPoint + 2 > tipEpoch
                    then pure $ increaseSyncSeconds oldCs
                    else consolidateWithStatus checkPoint oldCs epochSlots
        let newCs = fromMaybe (CSSyncSeconds 2) mcs
        sleepSeconds $ statusToSeconds newCs
        loop newCs

-- | Get a 'SerializedBlund' from the DB. If the block has already been
-- consolidated into an epoch file retieve it from there, otherwise,
-- retieve it using 'dbGetSerBlundRealDefault'.
dbGetConsolidatedSerBlundRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedBlund)
dbGetConsolidatedSerBlundRealDefault genesisHash hh = do
    bloc <- blundLocation genesisHash hh
    case bloc of
        BlundUnknown -> pure Nothing
        BlundFile -> dbGetSerBlundRealFile hh
        BlundEpoch sid ->
            Serialized . uncurry BS.append <<$>> getConsolidatedSerBlund sid

-- | Like 'dbGetConsolidatedSerBlundRealDefault' but for 'Block'.
dbGetConsolidatedSerBlockRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedBlock)
dbGetConsolidatedSerBlockRealDefault genesisHash hh = do
    bloc <- blundLocation genesisHash hh
    case bloc of
        BlundUnknown -> pure Nothing
        BlundFile -> dbGetSerBlockRealFile hh
        BlundEpoch sid ->
            Serialized . fst <<$>> getConsolidatedSerBlund sid

-- | Like 'dbGetConsolidatedSerBlundRealDefault' but for 'Undo'.
dbGetConsolidatedSerUndoRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => GenesisHash
    -> HeaderHash
    -> m (Maybe SerializedUndo)
dbGetConsolidatedSerUndoRealDefault genesisHash hh = do
    bloc <- blundLocation genesisHash hh
    case bloc of
        BlundUnknown -> pure Nothing
        BlundFile -> dbGetSerUndoRealFile hh
        BlundEpoch sid ->
            Serialized . snd <<$>> getConsolidatedSerBlund sid

-- -----------------------------------------------------------------------------

-- A (local to this module) data type to represent the location of a 'Blund'.
data BlundLocation
    = BlundUnknown          -- Lookup of hash failed
    | BlundFile             -- Blund is in a discrete file or is for an epoch boundary block.
    | BlundEpoch !SlotId    -- Blund is part of an epoch file at give SlotId.

-- Figure out where a 'Blund' is located. If the 'Blund' has been consolidated
-- into an epoch file, return its 'SlofId' within that epoch.
blundLocation :: MonadDBRead m => GenesisHash -> HeaderHash -> m BlundLocation
blundLocation genesisHash hh = do
    cei <- ccpEpochIndex <$> getConsolidateCheckPoint genesisHash
    meos <- getHeaderEpochOrSlot hh
    pure $ case unEpochOrSlot <$> meos of
            Nothing -> BlundUnknown
            Just (Left _) -> BlundFile -- Epoch boundary block.
            Just (Right sid) -> if siEpoch sid >= cei
                                    then BlundFile
                                    else BlundEpoch sid

-- | Get a SerializedBlund from an epoch file, using the 'SlotId's 'EpochIndex'
-- to determine which epoch file.
getConsolidatedSerBlund
    :: (MonadDBRead m, MonadRealDB ctx m)
    => SlotId -> m (Maybe (ByteString, ByteString))
getConsolidatedSerBlund (SlotId ei lsi) = do
    (epochPath, indexPath) <- mkEpochPaths ei . view epochDataDir <$> getNodeDBs
    moff <- liftIO $ getEpochBlundOffset indexPath lsi
    case moff of
        Nothing -> pure Nothing
        Just off -> do
            liftIO . withBinaryFile epochPath ReadMode $ \ hdl -> do
                hSeek hdl AbsoluteSeek $ fromIntegral off
                tag <- BS.hGet hdl 4
                if tag /= "blnd"
                    then pure Nothing
                    else do
                        blen1 <- fromIntegral . unpackWord32 <$> BS.hGet hdl 4
                        blen2 <- fromIntegral . unpackWord32 <$> BS.hGet hdl 4
                        blund <- (,) <$> BS.hGet hdl blen1 <*> BS.hGet hdl blen2
                        pure $ Just blund

-- -----------------------------------------------------------------------------

data ConsolidateError
    = CEExpectedMain !Text !HeaderHash
    | CEForwardLink !Text !HeaderHash
    | CEEoSLookupFailed !Text !HeaderHash
    | CEBlockLookupFailed !Text !LocalSlotIndex !HeaderHash
    | CEBBlockNotFound !Text !LocalSlotIndex !HeaderHash

renderConsolidateError :: ConsolidateError -> Text
renderConsolidateError = \case
    CEExpectedMain fn h ->
        fn <> sformat (": hash " % build % " should be a main block hash.") h
    CEForwardLink fn h ->
        fn <> sformat (": failed to follow hash " % build) h
    CEEoSLookupFailed fn h ->
        fn <> sformat (": EpochOrSlot lookup failed on hash " % build) h
    CEBlockLookupFailed fn lsi h ->
        fn <> sformat (": block lookup failed on (" % build % ", " % build % ")") lsi h
    CEBBlockNotFound fn lsi hh ->
        fn <> sformat (": block missing : " % build % " " % build) lsi hh

-- -----------------------------------------------------------------------------

data SlotIndexHash
    = SlotIndexHash !LocalSlotIndex !HeaderHash

data SlotIndexLength
    = SlotIndexLength !Word16 !Word32
    deriving Eq

-- For convenience in this module. Should not be allowed to escape.
type ConsolidateM ctx m =
    ( MonadIO m
    , MonadMask m
    , MonadRealDB ctx m
    , MonadDB m
    )

-- | A local (to this module) type to control the behaviour of the consolidation
-- process. A problem faced by this code is that there is no way for it to
-- figure out the current synchronisation status of the block chain because
-- that all happens at a higher level and monads/state that this code does not
-- have access to.
--
-- This code also needs to deal with a couple of scenarios on startup:
--  * This node is syncing the blockchain from scratch and needs to consolidate
--    older epochs as they arrive.
--  * This node is fully synced, but has been running old code and no
--    consolidation has ever been performed.
--  * This node is running code that does consolidate blocks and all but the
--    last two blocks have already been consolidated.
-- During any of the above secnarios, we may also need to deal with a flakey
-- network connection
--
-- To handle all three cases without any synchonisaion feedback, the best we can
-- do is poll the current tip epoch and compare it to our check point epoch, with
-- an increase in sleep times between polling. When the sleep time gets over a
-- certain amount we switch to a mode where this polling is done once every day.
-- When we are polling more frequently that once a day, if a epoch needs to be
-- consolidated, the sleep time resets to the minimum (2 seconds).
data ConsolidateStatus
    = CSSyncSeconds !Word
    -- ^ Consolodation is more than two blocks behind and needs to catch up.
    -- The 'Word' carries the number of seconds to sleep between consolidations.
    | CSFollowing
    -- ^ Consolodation is at most two blocks behind the tip so we poll once a
    -- a day.

statusToSeconds :: ConsolidateStatus -> Integer
statusToSeconds cs =
    case cs of
        CSSyncSeconds s -> fromIntegral s
        CSFollowing     -> 24 * 60 * 60 -- A whole day

-- Reset the sleep time to the minimum. When syncing  a large number of epochs,
-- with a large backlog of unconsolidated block, a fast network and a slow disk
-- can result epochs being consolidated slower than  they are retrieved from
-- the network.
resetSyncSeconds :: ConsolidateStatus -> ConsolidateStatus
resetSyncSeconds st =
    case st of
        CSSyncSeconds _ -> minimumSeconds
        CSFollowing     -> CSFollowing

-- | When the ConsolidateStatus is reset this is the minimum it gets reset to.
minimumSeconds :: ConsolidateStatus
minimumSeconds = CSSyncSeconds 2

-- When we increase the sleep time we do it slowly to allow for slow
-- syncing on slow networks, but when the last sleep time exceeds an hour
-- (cummulatively a little over 4 hours) we switch to polling once a day
-- (CSFollowing).
increaseSyncSeconds :: ConsolidateStatus -> ConsolidateStatus
increaseSyncSeconds st =
    case st of
        CSSyncSeconds s
            | s < 60 * 60 -> CSSyncSeconds (s + s `div` 3 + 1)
            | otherwise -> CSFollowing
        CSFollowing -> CSFollowing

consolidateWithStatus
    :: (HasLoggerName m, CanLog m, ConsolidateM ctx m)
    => ConsolidateCheckPoint -> ConsolidateStatus -> SlotCount -> m ConsolidateStatus
consolidateWithStatus checkPoint oldStatus epochSlots = do
    enCpp <- runExceptT $ consolidateOneEpoch checkPoint epochSlots
    case enCpp of
        Left e -> do
            logError $ renderConsolidateError e
            pure $ increaseSyncSeconds oldStatus -- Not much else to be done!
        Right () -> do
            -- 'tipEpoch' is the tip of the block chain that has been synced by this node so far.
            tipEpoch <- getTipEpoch
            logInfo $ sformat ("consolidated epoch "%int%", current tip is epoch "%int)
                    (getEpochIndex $ ccpEpochIndex checkPoint) (getEpochIndex tipEpoch)
            pure $ if ccpEpochIndex checkPoint + 2 > tipEpoch
                    then increaseSyncSeconds oldStatus
                    else resetSyncSeconds oldStatus


consolidateOneEpoch
    :: ConsolidateM ctx m
    => ConsolidateCheckPoint -> SlotCount -> ExceptT ConsolidateError m ()
consolidateOneEpoch ccp epochSlots = do
    (epochStart, sihs) <- getEpochHeaderHashes $ ccpHeaderHash ccp
    (epochPath, indexPath) <- mkEpochPaths (ccpEpochIndex ccp) . view epochDataDir <$> getNodeDBs

    xs <- consolidateEpochBlocks epochPath sihs
    liftIO $ writeEpochIndex epochSlots indexPath xs

    -- Write starting point for next consolidation to the MiscDB.
    putConsolidateCheckPoint $ ConsolidateCheckPoint (ccpEpochIndex ccp + 1) epochStart

    -- After the check point is written, delete old blunds for the epoch we have just
    -- consolidated.
    lift $ mapM_ deleter $ chunksOf 1000 sihs
  where
    deleter :: ConsolidateM ctx m => [SlotIndexHash] -> m ()
    deleter xs = do
        mapM_ deleteOldBlund xs
        sleepSeconds 2

deleteOldBlund :: ConsolidateM ctx m => SlotIndexHash -> m ()
deleteOldBlund (SlotIndexHash _ hh) = do
    bdd <- view blockDataDir <$> getNodeDBs
    let bp = bspBlund (getAllPaths bdd hh)
    liftIO (removeFile bp) `catch` handler
  where
    handler e
        | isDoesNotExistError e = pure ()
        | otherwise = throwM e


-- | Given a '[SlotIndexHash]' representing all the 'HeaderHash's for a given
-- epoch ordered by ascending 'LocalSlotIndex', write out a file containing all
-- the blocks to a single file specified by 'FilePath' and return a
-- '[SlotIndexOffset]' which is used to write the epoch index file.
consolidateEpochBlocks
    :: ConsolidateM ctx m
    => FilePath -> [SlotIndexHash] -> ExceptT ConsolidateError m [SlotIndexOffset]
consolidateEpochBlocks fpath xs = ExceptT $ do
    ys <- bracket
            (liftIO $ openBinaryFile fpath WriteMode)
            (liftIO . hClose)
            (\hdl -> do
                liftIO $ BS.hPutStr hdl epochFileHeader
                mapM (consolidate hdl) $ zip [0 .. ] xs
                )
    pure $ case partitionEithers ys of
            ([], zs) -> Right $ epochIndexToOffset zs
            (e:_, _) -> Left e
  where
    consolidate
        :: ConsolidateM ctx m
        => Handle -> (Int, SlotIndexHash) -> m (Either ConsolidateError SlotIndexLength)
    consolidate hdl  (indx, SlotIndexHash lsi hh) = do
        when (indx `mod` 1000 == 0) $
            sleepSeconds 2
        mblund <- getSerializedBlund hh
        case mblund of
            Nothing ->
                pure . Left $ CEBBlockNotFound "consolidateEpochBlocks" lsi hh
            Just (blck, undo) -> do
                let chunk = LBS.fromChunks
                        ["blnd"
                        , packWord32 $ fromIntegral (BS.length blck)
                        , packWord32 $ fromIntegral (BS.length undo)
                        , blck
                        , undo
                        ]
                liftIO $ LBS.hPutStr hdl chunk
                pure . Right $ SlotIndexLength (getSlotIndex lsi)
                                (fromIntegral $ LBS.length chunk)

-- | Get a list of headers for an epoch.
-- This function is designed to work on both Ouroboros classic (Original)
-- epochs and on Ouroboros BFT epochs. The only difference between these two
-- epoch types from the point of view of block consolidation is that Original
-- epochs start with an epoch boundary block (EBB) where as OBFT doesn't have
-- EBBs.
-- The inital header hash that is passed in should be the hash of either the
-- EBB for Original or of the zeroth block in the case of OBFT.
getEpochHeaderHashes
    :: MonadDBRead m
    => HeaderHash -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
getEpochHeaderHashes startHash = do
    -- Make sure the hash passed to the OBFT version is a Main block and not an
    -- epoch boundary block.
    next <- ifM (isMainBlockHeader startHash)
                (pure startHash)
                (maybe (throwE $ errorHash startHash) pure =<< resolveForwardLink startHash)

    -- For most epochs, the LocalSlotIndex here should be zero, but there is at
    -- least one exception to the rule, epoch 84 which suffered a chain stall
    -- and missed the first 772 slots.
    lsi <- getLocalSlotIndex next
    ei <- getBlockHeaderEpoch next
    (nh, bhs) <- loop ei [SlotIndexHash lsi next] next
    pure (nh, reverse bhs)
  where
    loop
        :: MonadDBRead m
        => EpochIndex -> [SlotIndexHash] -> HeaderHash
        -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
    loop currentEpoch !acc hash = do
        mnext <- resolveForwardLink hash
        next <- maybe (throwE $ errorHash hash) pure mnext
        nei <- getBlockHeaderEpoch next
        if nei /= currentEpoch
            then pure (next, acc)
            else do
                lsi <- getLocalSlotIndex next
                loop currentEpoch (SlotIndexHash lsi next : acc) next

    errorHash =
        CEForwardLink "getEpochHeaderHashes"

getLocalSlotIndex
    :: MonadDBRead m
    => HeaderHash -> ExceptT ConsolidateError m LocalSlotIndex
getLocalSlotIndex hh = do
    meos <- unEpochOrSlot <<$>> getHeaderEpochOrSlot hh
    case meos of
        Nothing          -> throwE $ CEEoSLookupFailed "getLocalSlotIndex" hh
        Just (Left _)    -> throwE $ CEExpectedMain "getLocalSlotIndex" hh
        Just (Right sid) -> pure $ siSlot sid

isMainBlockHeader :: MonadDBRead m => HeaderHash -> m Bool
isMainBlockHeader hh =
    maybe False (isRight . unEpochOrSlot) <$> getHeaderEpochOrSlot hh

getHeaderEpochOrSlot :: MonadDBRead m => HeaderHash -> m (Maybe EpochOrSlot)
getHeaderEpochOrSlot hh =
    getEpochOrSlot <<$>> getHeader hh

getTipEpoch :: MonadDBRead m => m EpochIndex
getTipEpoch =
    getBlockHeaderEpoch =<< fmap blockHeaderHash getTipHeader

getBlockHeaderEpoch :: MonadDBRead m => HeaderHash -> m EpochIndex
getBlockHeaderEpoch hhash = do
    meos <- unEpochOrSlot <<$>> getHeaderEpochOrSlot hhash
    case meos of
        Nothing -> throwM $ DBMalformed "getBlockHeaderEpoch: Nothing"
        Just (Left eid)  -> pure eid
        Just (Right sid) -> pure $ siEpoch sid


epochIndexToOffset :: [SlotIndexLength] -> [SlotIndexOffset]
epochIndexToOffset =
    snd . mapAccumL convert (fromIntegral $ BS.length epochFileHeader)
  where
    convert :: Word64 -> SlotIndexLength -> (Word64, SlotIndexOffset)
    convert offset (SlotIndexLength a b) =
        (offset + fromIntegral b, SlotIndexOffset a offset)

mkEpochPaths :: EpochIndex -> FilePath -> (FilePath, FilePath)
mkEpochPaths epoch dir =
    (dir </> epochStr ++ ".epoch", dir </> epochStr ++ ".index")
  where
    epochStr = replicate (5 - epochStrLen) '0' ++ epochShow
    epochShow = show $ getEpochIndex epoch
    epochStrLen = length epochShow

epochFileHeader :: ByteString
epochFileHeader = "Epoch data v1\n"

packWord32 :: Word32 -> ByteString
packWord32 = LBS.toStrict . encode

unpackWord32 :: ByteString -> Word32
unpackWord32 = decode . LBS.fromStrict

-- | Sleep for the required number of seconds. We use 'Integer' here to avoid
-- any chance of wrap around.
-- Code lifted from concurrent-extra version 0.5.
sleepSeconds :: MonadIO m => Integer -> m ()
sleepSeconds sec =
    liftIO . delay $ sec * 1000 * 1000
  where
    delay time = do
        let maxWait = min time $ toInteger (maxBound :: Int)
        liftIO $ threadDelay (fromInteger maxWait)
        when (maxWait /= time) $ delay (time - maxWait)

-- -----------------------------------------------------------------------------
-- ConsoldateCheckPoint is stored in the MiscDB and contains the EpochIndex
-- and HeaderHash of epoch boundary block that will be the next epoch to be
-- consilidated.

data ConsolidateCheckPoint = ConsolidateCheckPoint
    { ccpEpochIndex :: !EpochIndex
      -- ^ The EpochIndex of the next epoch to be consolidated.
    , ccpHeaderHash :: !HeaderHash
      -- ^ The HeaderHash of first block of the next epoch. In the case of
      -- Ouroboros Original/Classic, this will be the epoch boundary block
      -- and in the case of OBFT, this with be the zeroth block of the next
      -- epoch.
    }

-- | Get the 'HeaderHash' of the marking the start of the first un-consolidated
-- epoch. If no epoch consolidation has happened, then return the HeaderHash
-- of the original genesis block.
-- This can fail (due to getFirstGenesisBlockHash failing) if the database
-- context is not set up correctly.
getConsolidateCheckPoint :: MonadDBRead m => GenesisHash -> m ConsolidateCheckPoint
getConsolidateCheckPoint genesisHash =
   miscGetBi consolidateCheckPointKey >>= \case
        Just eh -> pure eh
        Nothing -> ConsolidateCheckPoint 0 <$> getFirstGenesisBlockHash genesisHash

-- | Store the consolidation check point.
putConsolidateCheckPoint :: MonadDB m => ConsolidateCheckPoint -> m ()
putConsolidateCheckPoint =
    miscPutBi consolidateCheckPointKey

consolidateCheckPointKey :: ByteString
consolidateCheckPointKey = "consolidateCheckPoint"

-- -----------------------------------------------------------------------------
-- TH at the end of the file.

-- ConsolidateCheckPoint gets stored in the MiscDB so we need a 'Bi' instance.
deriveSimpleBi ''ConsolidateCheckPoint [
    Cons 'ConsolidateCheckPoint [
        Field [| ccpEpochIndex :: EpochIndex |],
        Field [| ccpHeaderHash :: HeaderHash |]
    ]]
