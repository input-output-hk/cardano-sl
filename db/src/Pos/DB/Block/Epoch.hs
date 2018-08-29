{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.DB.Block.Epoch
       ( ConsolidateError (..)
       , consoldidateEpochs
       , dbGetConsolidatedSerBlundRealDefault
       , dbGetConsolidatedSerBlockRealDefault
       , dbGetConsolidatedSerUndoRealDefault
       , renderConsolidateError

       -- TODO: Public during development/testing.
       , ConsolidateCheckPoint (..)
       , deleteConsolidateCheckPoint
       , getConsolidateCheckPoint
       ) where

import           Universum

import           Control.Monad (when)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Binary (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (partitionEithers)
import           Formatting (build, sformat, (%))
import           System.Directory (renameFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (..), SeekMode (..), hClose, hSeek,
                     openBinaryFile, withBinaryFile)
import           System.IO.Error (isDoesNotExistError)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Block (HeaderHash, blockHeaderHash)
import           Pos.Core as Core (Config (..), EpochIndex (..),
                     EpochOrSlot (..), LocalSlotIndex (..), SlotCount (..),
                     SlotId (..), configEpochSlots, getEpochOrSlot)
import           Pos.DB.Block.GState.BlockExtra (getFirstGenesisBlockHash,
                     resolveForwardLink)
import           Pos.DB.Block.Internal (bspBlund, dbGetSerBlockRealFile,
                     dbGetSerBlundRealFile, dbGetSerUndoRealFile, getAllPaths,
                     getSerializedBlund)
import           Pos.DB.BlockIndex (getHeader, getTipHeader)
import           Pos.DB.Class (DBTag (MiscDB), MonadDB (..), MonadDBRead (..),
                     Serialized (..), SerializedBlock, SerializedBlund,
                     SerializedUndo)
import           Pos.DB.Epoch.Index (SlotIndexOffset (..), getEpochBlundOffset,
                     writeEpochIndex)
import           Pos.DB.Misc.Common (miscGetBi, miscPutBi)
import           Pos.DB.Rocks.Types (MonadRealDB, NodeDBs (..), blockDataDir,
                     epochDataDir, epochLock, getNodeDBs)
import           Pos.Util.Concurrent.RWLock (whenAcquireWrite)

data ConsolidateError
    = CEFinalBlockNotBoundary !Text
    | CEExpectedGenesis !Text !HeaderHash
    | CEExcpectedMain !Text !HeaderHash
    | CEForwardLink !Text !HeaderHash
    | CEEoSLookupFailed !Text !HeaderHash
    | CEBlockLookupFailed !Text !LocalSlotIndex !HeaderHash
    | CEBOffsetFail !Text
    | CEBlockMismatch !Text !LocalSlotIndex
    | CEBBlockNotFound !Text !LocalSlotIndex !HeaderHash

renderConsolidateError :: ConsolidateError -> Text
renderConsolidateError = \case
    CEFinalBlockNotBoundary fn ->
        fn <> ": Final block is not an epoch boundary block"
    CEExpectedGenesis fn h ->
        fn <> sformat (": hash " % build % " should be an epoch boundary hash.") h
    CEExcpectedMain fn h ->
        fn <> sformat (": hash " % build % " should be a main block hash.") h
    CEForwardLink fn h ->
        fn <> sformat (": failed to follow hash " % build) h
    CEEoSLookupFailed fn h ->
        fn <> sformat (": EpochOrSlot lookup failed on hash " % build) h
    CEBlockLookupFailed fn lsi h ->
        fn <> sformat (": block lookup failed on (" % build % ", " % build % ")") lsi h
    CEBOffsetFail fn ->
        fn <> ": Failed to find offset"
    CEBlockMismatch fn lsi ->
        fn <> sformat (": block mismatch at index " % build) lsi
    CEBBlockNotFound fn lsi hh ->
        fn <> sformat (": block mssing : " % build % " " % build) lsi hh

-- | Consolidate discrete blund files for a single epoch into a single epoch/
-- index file pair. Will consolidate from the original genesis epoch up to and
-- including two epochs before the current epoch.
-- If a consolidation has been done before it will retrieve a check point
-- from the MisDB and start from there.
-- This function takes and holds a RWLock to ensure that only one consolidation
-- is running at any time.
consoldidateEpochs
    :: (MonadCatch m, MonadDB m, MonadIO m, MonadMask m, MonadReader NodeDBs m)
    => Core.Config -> ExceptT ConsolidateError m ()
consoldidateEpochs coreConfig = ExceptT $ do
    elock <- view epochLock <$> getNodeDBs
    mr <- whenAcquireWrite elock $ do
            tipEpoch <- getTipEpoch
            checkPoint <- getConsolidateCheckPoint
            if tipEpoch < 2
                then pure $ Right ()
                else consolidateLoop checkPoint (configEpochSlots coreConfig) (tipEpoch - 1)
    pure $ fromMaybe (Right ()) mr


-- | Get a 'SerializedBlund' from the DB. If the block has already been
-- consolidated into an epoch file retieve it from there, otherwise,
-- retieve it using 'dbGetSerBlundRealDefault'.
dbGetConsolidatedSerBlundRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlund)
dbGetConsolidatedSerBlundRealDefault hh = do
    bloc <- blundLocation hh
    case bloc of
        BlundUnknown -> pure Nothing
        BlundFile -> dbGetSerBlundRealFile hh
        BlundEpoch sid ->
            Serialized . uncurry BS.append <<$>> getConsolidatedSerBlund sid

-- | Like 'dbGetConsolidatedSerBlundRealDefault' but for 'Block'.
dbGetConsolidatedSerBlockRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetConsolidatedSerBlockRealDefault hh = do
    bloc <- blundLocation hh
    case bloc of
        BlundUnknown -> pure Nothing
        BlundFile -> dbGetSerBlockRealFile hh
        BlundEpoch sid ->
            Serialized . fst <<$>> getConsolidatedSerBlund sid

-- | Like 'dbGetConsolidatedSerBlundRealDefault' but for 'Undo'.
dbGetConsolidatedSerUndoRealDefault
    :: (MonadDBRead m, MonadRealDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetConsolidatedSerUndoRealDefault hh = do
    bloc <- blundLocation hh
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
blundLocation :: MonadDBRead m => HeaderHash -> m BlundLocation
blundLocation hh = do
    cei <- ccpEpochIndex <$> getConsolidateCheckPoint
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

data SlotIndexHash
    = SlotIndexHash !LocalSlotIndex !HeaderHash

data SlotIndexLength
    = SlotIndexLength !Word16 !Word32
    deriving Eq

-- For convenience in this module. Should not be allowed to escape.
type ConsolidateM m =
    ( MonadIO m
    , MonadMask m
    , MonadReader NodeDBs m
    , MonadDB m
    )

-- Consolidate from the check point until the specified 'endEpoch'. If
-- the 'endEpoch' has already been consolidated, return success.
-- On each loop, the check point in the 'MiscDB' is updated.
consolidateLoop
    :: ConsolidateM m
    => ConsolidateCheckPoint -> SlotCount -> EpochIndex -> m (Either ConsolidateError ())
consolidateLoop startCcp epochSlots endEpoch
    | ccpEpochIndex startCcp >= endEpoch = pure $ Right ()
    | otherwise = runExceptT $ loop startCcp
  where
    loop
        :: ConsolidateM m
        => ConsolidateCheckPoint -> ExceptT ConsolidateError m ()
    loop ccp = do
        (epochBoundary, sihs) <- getEpochHeaderHashes $ ccpHeaderHash ccp
        (epochPath, indexPath) <- mkEpochPaths (ccpEpochIndex ccp) . view epochDataDir <$> getNodeDBs
        liftIO $ print (epochPath, indexPath)

        xs <- consolidateEpochBlocks epochPath sihs
        liftIO $ writeEpochIndex epochSlots indexPath xs

        -- Write starting point for next consolidation to the MiscDB.
        let nextCcp = ConsolidateCheckPoint (ccpEpochIndex ccp + 1) epochBoundary
        putConsolidateCheckPoint nextCcp

        -- After the check point is written, delete old Blunds.
        lift $ mapM_ deleteOldBlund sihs

        when (ccpEpochIndex nextCcp < endEpoch) $
            loop nextCcp

deleteOldBlund :: ConsolidateM m => SlotIndexHash -> m ()
deleteOldBlund (SlotIndexHash _ hh) = do
    bdd <- view blockDataDir <$> getNodeDBs
    let bp = bspBlund (getAllPaths bdd hh)
    -- TODO: During development/testing, we rename rather than delete.
    (liftIO $ renameFile bp (bp ++ ".bak")) `catch` handler
  where
    handler e
        | isDoesNotExistError e = pure ()
        | otherwise = throwM e


-- | Given a '[SlotIndexHash]' representing all the 'HeaderHash's for a given
-- epoch ordered by ascending 'LocalSlotIndex', write out a file containing all
-- the blocks to a single file specified by 'FilePath' and return a
-- '[SlotIndexOffset]' which is used to write the epoch index file.
consolidateEpochBlocks
    :: ConsolidateM m
    => FilePath -> [SlotIndexHash] -> ExceptT ConsolidateError m [SlotIndexOffset]
consolidateEpochBlocks fpath xs = ExceptT $ do
    ys <- bracket
            (liftIO $ openBinaryFile fpath WriteMode)
            (liftIO . hClose)
            (\hdl -> do
                liftIO $ BS.hPutStr hdl epochFileHeader
                mapM (consolidate hdl) xs
                )
    pure $ case partitionEithers ys of
            ([], zs) -> Right $ epochIndexToOffset zs
            (e:_, _) -> Left e
  where
    consolidate
        :: ConsolidateM m
        => Handle -> SlotIndexHash -> m (Either ConsolidateError SlotIndexLength)
    consolidate hdl  (SlotIndexHash lsi hh) = do
        mblund <- getSerializedBlund hh
        case mblund of
            Nothing ->
                pure . Left $ CEBBlockNotFound "consolidateEpochBlocks" lsi hh
            Just (blck, undo) -> do
                liftIO $ do
                    LBS.hPutStr hdl $ LBS.fromChunks
                        ["blnd"
                        , packWord32 $ fromIntegral (BS.length blck)
                        , packWord32 $ fromIntegral (BS.length undo)
                        , blck
                        , undo
                        ]
                pure . Right $
                    SlotIndexLength (getSlotIndex lsi)
                        (fromIntegral $ 12 + BS.length blck + BS.length undo)

-- | Given the hash of an epoch boundary block, return a pair of the next
-- epoch boundary hash and a list of the header hashes of the main blocks
-- between the two boundary blocks.
getEpochHeaderHashes
    :: MonadDBRead m
    => HeaderHash -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
getEpochHeaderHashes ghash = do
    mbh <- isMainBlockHeader ghash
    when mbh $
        throwE $ CEExpectedGenesis "getEpochHeaderHashes" ghash
    (ng, bhs) <- loop [] ghash
    whenM (isMainBlockHeader ng) $
        throwE $ CEFinalBlockNotBoundary "getEpochHeaderHashes"
    pure (ng, reverse bhs)
  where
    loop
        :: MonadDBRead m
        => [SlotIndexHash] -> HeaderHash
        -> ExceptT ConsolidateError m (HeaderHash, [SlotIndexHash])
    loop !acc hash = do
        mnext <- resolveForwardLink hash
        next <- maybe (throwE $ errorHash hash) pure mnext
        ifM (not <$> isMainBlockHeader next)
            (pure (next, acc))
            (do lsi <- getLocalSlotIndex next
                loop (SlotIndexHash lsi next : acc) next
                )

    errorHash hash =
        CEForwardLink "getEpochHeaderHashes" hash

getLocalSlotIndex
    :: MonadDBRead m
    => HeaderHash -> ExceptT ConsolidateError m LocalSlotIndex
getLocalSlotIndex hh = do
    meos <- getHeaderEpochOrSlot hh
    case meos of
        Nothing -> throwE $ CEEoSLookupFailed "getLocalSlotIndex" hh
        Just eos ->
            case unEpochOrSlot eos of
                Left _    -> throwE $ CEExcpectedMain "getLocalSlotIndex" hh
                Right sid -> pure $ siSlot sid

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
    meos <- getHeaderEpochOrSlot hhash
    case meos of
        Nothing -> error "getBlockHeaderEpoch: Nothing"
        Just eos ->
            case unEpochOrSlot eos of
                Left eid  -> pure eid
                Right sid -> pure $ siEpoch sid


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

-- -----------------------------------------------------------------------------
-- ConsoldateCheckPoint is stored in the MiscDB and contains the EpochIndex
-- and HeaderHash of epoch boundary block that will be the next epoch to be
-- consilidated.

data ConsolidateCheckPoint = ConsolidateCheckPoint
    { ccpEpochIndex :: !EpochIndex
    , ccpHeaderHash :: !HeaderHash
    }

-- | Get the 'HeaderHash' of the marking the start of the first un-consolidated
-- epoch. If no epoch consolidation has happened, then return the HeaderHash
-- of the original genesis block.
-- This can fail (due to getFirstGenesisBlockHash failing) if the database
-- context is not set up correctly.
getConsolidateCheckPoint :: MonadDBRead m => m ConsolidateCheckPoint
getConsolidateCheckPoint =
   miscGetBi consolidateCheckPointKey >>= \case
        Just eh -> pure eh
        Nothing -> ConsolidateCheckPoint 0 <$> getFirstGenesisBlockHash

-- | Store the hash of the epoch boundary block which is at the start of the
-- next epoch to be consolidated.
putConsolidateCheckPoint :: MonadDB m => ConsolidateCheckPoint -> m ()
putConsolidateCheckPoint =
    miscPutBi consolidateCheckPointKey

-- | Strictly for testing before we add code to delete blund files after they
-- have been consolidated.
deleteConsolidateCheckPoint :: MonadDB m => m ()
deleteConsolidateCheckPoint =
    dbDelete MiscDB consolidateCheckPointKey


consolidateCheckPointKey :: ByteString
consolidateCheckPointKey = "consolidateCheckPoint"

-- -----------------------------------------------------------------------------
-- TH at the end of the file.

deriveSimpleBi ''ConsolidateCheckPoint [
    Cons 'ConsolidateCheckPoint [
        Field [| ccpEpochIndex :: EpochIndex |],
        Field [| ccpHeaderHash :: HeaderHash |]
    ]]
