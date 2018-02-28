{-# LANGUAGE RankNTypes #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Universum

import           Conduit ((.|))
import qualified Conduit as C
import           Control.Concurrent.STM (putTMVar, swapTMVar, tryReadTBQueue, tryReadTMVar,
                                         tryTakeTMVar)
import           Control.Exception.Safe (handleAny)
import           Control.Lens (to)
import           Control.Monad.STM (retry)
import qualified Data.Conduit.Async as Conduit.Async
import qualified Data.List.NonEmpty as NE
import           Ether.Internal (HasLens (..))
import           Fmt (format, (+|), (|+))
import           Formatting (build, int, sformat, (%))
import           Mockable (delay)
import qualified Network.HTTP.Simple as Http
import           Serokell.Util (sec)
import           System.Wlog (logDebug, logError, logInfo, logWarning)

import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Dump (decodeBlockDumpC)
import           Pos.Block.Logic (ClassifyHeaderRes (..), classifyNewHeader, getHeadersOlderExp,
                                  verifyAndApplyBlocksC)
import           Pos.Block.Network.Logic (BlockNetLogicException (DialogUnexpected), handleBlocks,
                                          triggerRecovery)
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..))
import           Pos.Block.RetrievalQueue (BlockRetrievalQueueTag, BlockRetrievalTask (..))
import           Pos.Block.Types (RecoveryHeaderTag)
import           Pos.Communication.Protocol (NodeId, OutSpecs, convH, toOutSpecs)
import           Pos.Core (Block, EpochIndex, HasHeaderHash (..), HeaderHash, difficultyL,
                           epochIndexL, getEpochOrSlot, isMoreDifficult, prevBlockL, siSlotL,
                           unEpochOrSlot)
import           Pos.Core.Block (BlockHeader)
import           Pos.Crypto (shortHashF)
import qualified Pos.DB.BlockIndex as DB
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion (Diffusion (getBlocks))
import           Pos.Reporting (reportOrLogE, reportOrLogW)
import           Pos.Slotting.Class (getCurrentSlot)
import           Pos.StateLock (Priority (HighPriority), modifyStateLock)
import           Pos.Util ((<//>))
import           Pos.Util.Chrono (NE, OldestFirst (..), _OldestFirst)
import           Pos.Worker.Types (WorkerSpec, worker)

retrievalWorker
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => Maybe Text -> (WorkerSpec m, OutSpecs)
retrievalWorker blockStorageMirror =
    worker outs (retrievalWorkerImpl blockStorageMirror)
  where
    outs = toOutSpecs [convH (Proxy :: Proxy MsgGetBlocks)
                             (Proxy :: Proxy MsgBlock)
                      ]

-- I really don't like join
{-# ANN retrievalWorkerImpl ("HLint: ignore Use join" :: Text) #-}

-- | Worker that queries blocks. It has two jobs:
--
-- * If there are headers in 'BlockRetrievalQueue', this worker retrieves
--   blocks according to that queue.
--
-- * If recovery is in progress, this worker keeps recovery going by asking
--   headers (and then switching to block retrieval on next loop iteration).
--
-- If both happen at the same time, 'BlockRetrievalQueue' takes precedence.
--
-- Note: if we haven't caught up with the blockchain yet, the worker will
-- try to download and apply blockchain dumps, and only if it fails it'll
-- switch to requesting headers/blocks.
--
retrievalWorkerImpl
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => Maybe Text -> Diffusion m -> m ()
retrievalWorkerImpl blockStorageMirror diffusion = do
    whenJust blockStorageMirror $ \dumpUrl ->
        handleAny downloadBlockDumpE $ do
            epoch <- view epochIndexL <$> DB.getTipHeader
            downloadBlockDump epoch (toString dumpUrl)
    -- It would make some sense to do HTTP sync inside the 'retrievalLoop'
    -- (either we download blocks from a node or from a server), but
    --
    -- 1. I don't feel confident enough about 'retrievalLoop' to modify it
    -- 2. hopefully, soon enough we'll get rid of HTTP sync
    handleAny retrievalLoopE $ do
        logInfo "Starting retrieval loop"
        retrievalLoop
  where
    downloadBlockDumpE e =
        reportOrLogE "retrievalWorker downloadBlockDumpE: error caught " e

    retrievalLoop = do
        queue        <- view (lensOf @BlockRetrievalQueueTag)
        recHeaderVar <- view (lensOf @RecoveryHeaderTag)
        logDebug "Waiting on the block queue or recovery header var"
        -- Reading the queue is a priority, because it sets the recovery
        -- variable in case the header is classified as alternative. So if the
        -- queue contains lots of headers after a long delay, we'll first
        -- iterate over them and set recovery variable to the latest one, and
        -- only then we'll do recovery.
        thingToDoNext <- atomically $ do
            mbQueuedHeadersChunk <- tryReadTBQueue queue
            mbRecHeader <- tryReadTMVar recHeaderVar
            case (mbQueuedHeadersChunk, mbRecHeader) of
                (Nothing, Nothing) -> retry
                -- Dispatch the task
                (Just (nodeId, task), _) ->
                    pure (handleBlockRetrieval nodeId task)
                -- No tasks & the recovery header is set => do the recovery
                (_, Just (nodeId, rHeader))  ->
                    pure (handleRecoveryWithHandler nodeId rHeader)
        () <- thingToDoNext
        retrievalLoop
    retrievalLoopE e = do
        -- REPORT:ERROR 'reportOrLogE' in block retrieval worker.
        reportOrLogE "retrievalWorker retrievalLoopE: error caught " e
        delay (sec 1)
        retrievalLoop

    -----------------

    -- That's the first queue branch (task dispatching).
    handleBlockRetrieval nodeId BlockRetrievalTask{..} = do
        logDebug $ sformat
            ("Block retrieval queue task received, nodeId="%build%
             ", header="%build%", continues="%build)
            nodeId
            (headerHash brtHeader)
            brtContinues
        (if brtContinues then handleContinues else handleAlternative)
            nodeId
            brtHeader

    -- When we have a continuation of the chain, just try to get and apply it.
    handleContinues nodeId header = do
        let hHash = headerHash header
        logDebug $ "handleContinues: " <> pretty hHash
        classifyNewHeader header >>= \case
            CHContinues ->
                void $ getProcessBlocks diffusion nodeId header [hHash]
            res -> logDebug $
                "processContHeader: expected header to " <>
                "be continuation, but it's " <> show res

    -- When we have an alternative header, we should check whether it's actually
    -- recovery mode (server side should send us headers as a proof) and then
    -- enter recovery mode.
    handleAlternative nodeId header = do
        logDebug $ "handleAlternative: " <> pretty (headerHash header)
        classifyNewHeader header >>= \case
            CHInvalid _ ->
                logError "handleAlternative: invalid header got into retrievalWorker queue"
            CHUseless _ ->
                logDebug $
                sformat ("handleAlternative: header "%build%" became useless, ignoring it")
                        header
            _ -> do
                logDebug "handleAlternative: considering header for recovery mode"
                -- CSL-1514
                updateRecoveryHeader (Just nodeId) header

    -----------------

    handleRecoveryWithHandler nodeId rHeader =
        handleAny (handleRecoveryE nodeId rHeader) $
        handleRecovery nodeId rHeader

    -- We immediately drop recovery mode/header and request tips
    -- again.
    handleRecoveryE nodeId rHeader e = do
        -- REPORT:ERROR 'reportOrLogW' in block retrieval worker/recovery.
        reportOrLogW (sformat
            ("handleRecoveryE: error handling nodeId="%build%", header="%build%": ")
            nodeId (headerHash rHeader)) e
        dropRecoveryHeaderAndRepeat diffusion nodeId

    -- Recovery handling. We assume that header in the recovery var is
    -- appropriate and just query headers/blocks.
    handleRecovery :: Maybe NodeId -> BlockHeader -> m ()
    handleRecovery Nothing _ = do
        logWarning "handleRecovery: unexpected 'Nothing' in the recovery \
                   \header (we should only have Nothing there when we're \
                   \doing HTTP sync, but something went wrong)"
        dropRecoveryHeaderAndRepeat diffusion Nothing
    handleRecovery (Just nodeId) rHeader = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will fetch more blocks"
        whenM (fmap isJust $ DB.getHeader $ headerHash rHeader) $
            -- How did we even get into recovery then?
            throwM $ DialogUnexpected $ "handleRecovery: recovery header is " <>
                                        "already present in db"
        logDebug "handleRecovery: fetching blocks"
        checkpoints <- toList <$> getHeadersOlderExp Nothing
        void $ getProcessBlocks diffusion nodeId rHeader checkpoints

----------------------------------------------------------------------------
-- HTTP-based block retrieval
----------------------------------------------------------------------------

-- | Try to download and apply blockchain dumps, starting from the given
-- epoch. Halts after having downloaded enough dumps for the current slot to
-- become known.
downloadBlockDump
    :: forall ctx m. BlockWorkMode ctx m
    => EpochIndex  -- ^ The first epoch for which to download blocks
    -> String      -- ^ URL of a folder with block dumps
    -> m ()
downloadBlockDump epoch dumpUrl = whenNothingM_ getCurrentSlot $ do
    let epochUrl = dumpUrl <//> format "epoch{}.cbor.lzma" (toInteger epoch)
    logInfo ("Downloading blockchain dump for epoch "+|epoch|+
             " from "+|epochUrl|+"")
    isSuccess <- modifyStateLock HighPriority "tryDownload" $ \tip -> do
        updateRecoveryHeader Nothing =<< DB.getTipHeader
        slot <- getEpochOrSlot <$> DB.getTipHeader
        -- When we're already in the epoch we're going to be downloading
        -- blocks from, it's possible that we're in slot 10000 or
        -- something and we'll have to skip 10000 blocks before we can
        -- show any progress. In this case let's warn about it.
        whenRight (unEpochOrSlot slot) $ \slotId ->
            when (slotId ^. epochIndexL == epoch) $
                logDebug ("Going to download blocks and skip until slot "
                          +|view siSlotL slotId|+", this may take some time")
        let applyDump
                = decodeBlockDumpC
               .| (C.dropWhileC ((/= tip) . view prevBlockL)
               >> C.transPipe lift (verifyAndApplyBlocksC True))
        -- We use Data.Conduit.Async to keep a buffer of downloaded data and
        -- apply blocks in parallel with downloading them. The buffer size
        -- is 32 kB * 320 = 10 MB.
        --
        -- (32 kB is the default chunk size from Data.ByteString.Lazy and 10
        -- MB is approximate epoch size. Also, 10 MB is one order of
        -- magnitude less than the memory usually used by the node.)
        request <- Http.parseRequest epochUrl
        (mbErr, newTip) <- C.runResourceT $
            Conduit.Async.buffer 320
                (Http.httpSource request Http.getResponseBody
                 .| C.chunksOfCE 32768)
                applyDump
        updateRecoveryHeader Nothing =<< DB.getTipHeader
        -- TODO: abort if downloading is too slow and we haven't gotten
        -- any blocks in e.g. last minute
        case mbErr of
            Just err -> do
                logError ("Failed to apply the dump: "+|err|+"")
                pure (newTip, False)
            Nothing ->
                pure (newTip, True)
    when isSuccess $ downloadBlockDump (succ epoch) dumpUrl

----------------------------------------------------------------------------
-- Entering and exiting recovery mode
----------------------------------------------------------------------------

-- | Result of attempt to update recovery header.
data UpdateRecoveryResult
    = RecoveryStarted (Maybe NodeId) BlockHeader
      -- ^ Recovery header was absent, so we've set it.
    | RecoveryShifted
        { urrOldNodeId :: Maybe NodeId
        , urrOldHeader :: BlockHeader
        , urrNewNodeId :: Maybe NodeId
        , urrNewHeader :: BlockHeader
        }
      -- ^ Header was present, but we've replaced it with another
      -- (more difficult) one.
    | RecoveryContinued (Maybe NodeId) BlockHeader
      -- ^ Header is good, but is irrelevant, so recovery variable is
      -- unchanged.

-- | Be careful to run this in the same thread that ends recovery mode
-- (or synchronise those threads with an MVar), otherwise a race
-- condition can occur where we are caught in the recovery mode
-- indefinitely.
updateRecoveryHeader
    :: BlockWorkMode ctx m
    => Maybe NodeId
    -> BlockHeader
    -> m ()
updateRecoveryHeader nodeId hdr = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
    logDebug "Updating recovery header..."
    updated <- atomically $ do
        mbRecHeader <- tryReadTMVar recHeaderVar
        case mbRecHeader of
            Nothing -> do
                putTMVar recHeaderVar (nodeId, hdr)
                return $ RecoveryStarted nodeId hdr
            Just (oldNodeId, oldHdr) -> do
                let needUpdate = hdr `isMoreDifficult` oldHdr
                if needUpdate
                    then swapTMVar recHeaderVar (nodeId, hdr) $>
                         RecoveryShifted
                           { urrOldNodeId = oldNodeId
                           , urrOldHeader = oldHdr
                           , urrNewNodeId = nodeId
                           , urrNewHeader = hdr }
                    else return $ RecoveryContinued oldNodeId oldHdr
    logDebug $ case updated of
        RecoveryStarted rNodeId rHeader -> sformat
            ("Recovery started with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)
        RecoveryShifted{..} -> sformat
            ("Recovery shifted from nodeId="%build%" and tip="%build%
             " to nodeId="%build%" and tip="%build)
            urrOldNodeId (headerHash urrOldHeader)
            urrNewNodeId (headerHash urrNewHeader)
        RecoveryContinued rNodeId rHeader -> sformat
            ("Recovery continued with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)

-- | The returned 'Bool' signifies whether given peer was kicked and recovery
-- was stopped.
--
-- NB. The reason @nodeId@ is passed is that we want to avoid a race
-- condition. If you work with peer P and header H, after failure you want to
-- drop communication with P; however, if at the same time a new block
-- arrives and another thread replaces peer and header to (P2, H2), you want
-- to continue working with P2 and ignore the exception that happened with P.
-- So, @nodeId@ is used to check that the peer wasn't replaced mid-execution.
dropRecoveryHeader
    :: BlockWorkMode ctx m
    => Maybe NodeId
    -> m Bool
dropRecoveryHeader nodeId = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
    (kicked,realPeer) <- atomically $ do
        let processKick (peer,_) = do
                let p = peer == nodeId
                when p $ void $ tryTakeTMVar recHeaderVar
                pure (p, Just peer)
        maybe (pure (True,Nothing)) processKick =<< tryReadTMVar recHeaderVar
    when kicked $ logWarning $
        sformat ("Recovery mode communication dropped with peer "%build) nodeId
    unless kicked $
        logDebug $ "Recovery mode wasn't disabled: " <>
                   maybe "noth" show realPeer <> " vs " <> show nodeId
    pure kicked

-- | Drops the recovery header and, if it was successful, queries the tips.
dropRecoveryHeaderAndRepeat
    :: (BlockWorkMode ctx m)
    => Diffusion m -> Maybe NodeId -> m ()
dropRecoveryHeaderAndRepeat diffusion nodeId = do
    kicked <- dropRecoveryHeader nodeId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logDebug "Attempting to restart recovery"
        delay $ sec 2
        handleAny handleRecoveryTriggerE $ triggerRecovery diffusion
        logDebug "Attempting to restart recovery over"
    handleRecoveryTriggerE =
        -- REPORT:ERROR 'reportOrLogE' somewhere in block retrieval.
        reportOrLogE $ "Exception happened while trying to trigger " <>
                       "recovery inside dropRecoveryHeaderAndRepeat: "

-- Returns only if blocks were successfully downloaded and
-- processed. Throws exception if something goes wrong.
getProcessBlocks
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => Diffusion m
    -> NodeId
    -> BlockHeader
    -> [HeaderHash]
    -> m ()
getProcessBlocks diffusion nodeId desired checkpoints = do
    result <- Diffusion.getBlocks diffusion nodeId desired checkpoints
    case OldestFirst <$> nonEmpty (getOldestFirst result) of
      Nothing -> do
          let msg = sformat ("getProcessBlocks: diffusion returned []"%
                             " on request to fetch "%shortHashF%" from peer "%build)
                            (headerHash desired) nodeId
          throwM $ DialogUnexpected msg
      Just (blocks :: OldestFirst NE Block) -> do
          recHeaderVar <- view (lensOf @RecoveryHeaderTag)
          logDebug $ sformat
              ("Retrieved "%int%" blocks")
              (blocks ^. _OldestFirst . to NE.length)
          handleBlocks nodeId blocks diffusion
          -- If we've downloaded any block with bigger
          -- difficulty than ncRecoveryHeader, we're
          -- gracefully exiting recovery mode.
          let isMoreDifficultThan b x = b ^. difficultyL >= x ^. difficultyL
          exitedRecovery <- atomically $ tryReadTMVar recHeaderVar >>= \case
              -- We're not in recovery mode? That must be ok.
              Nothing -> pure False
              -- If we're in recovery mode we should exit it if
              -- any block is more difficult than one in
              -- recHeader.
              Just (_, rHeader) ->
                  if any (`isMoreDifficultThan` rHeader) blocks
                  then isJust <$> tryTakeTMVar recHeaderVar
                  else pure False
          when exitedRecovery $
              logInfo "Recovery mode exited gracefully on receiving block we needed"
