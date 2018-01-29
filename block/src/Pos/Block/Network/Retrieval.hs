{-# LANGUAGE RankNTypes #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Universum

import           Control.Concurrent.STM (putTMVar, swapTMVar, tryReadTBQueue, tryReadTMVar,
                                         tryTakeTMVar)
import           Control.Exception.Safe (handleAny)
import           Control.Lens (to)
import           Control.Monad.STM (retry)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (toMicroseconds)
import           Ether.Internal (HasLens (..))
import           Formatting (build, builder, int, sformat, (%), shown)
import           Mockable (delay)
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util (sec)
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logError, logInfo, logWarning)

import           Pos.Binary.Class (biSize)
import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Logic (ClassifyHeaderRes (..), classifyNewHeader)
import           Pos.Block.Network.Logic (BlockNetLogicException (DialogUnexpected),
                                          MkHeadersRequestResult (..), handleBlocks,
                                          mkHeadersRequest, triggerRecovery)
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..))
import           Pos.Block.RetrievalQueue (BlockRetrievalQueueTag, BlockRetrievalTask (..))
import           Pos.Block.Types (ProgressHeaderTag, RecoveryHeaderTag)
import           Pos.Communication.Protocol (NodeId, OutSpecs, convH, toOutSpecs)
import           Pos.Core (HasHeaderHash (..), HeaderHash, difficultyL, isMoreDifficult)
import           Pos.Core.Block (BlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion (Diffusion (getBlocks))
import           Pos.Reporting (reportOrLogE, reportOrLogW)
import           Pos.Slotting.Util (getCurrentEpochSlotDuration)
import           Pos.Util.Chrono (OldestFirst (..), _OldestFirst)
import           Pos.Util.Timer (Timer, setTimerDuration, startTimer)
import           Pos.Worker.Types (WorkerSpec, worker)

retrievalWorker
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => Timer -> (WorkerSpec m, OutSpecs)
retrievalWorker keepAliveTimer = worker outs (retrievalWorkerImpl keepAliveTimer)
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
retrievalWorkerImpl
    :: forall ctx m.
       (BlockWorkMode ctx m)
    => Timer -> Diffusion m -> m ()
retrievalWorkerImpl keepAliveTimer diffusion =
    handleAny mainLoopE $ do
        logInfo "Starting retrievalWorker loop"
        mainLoop
  where
    mainLoop = do
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
        -- Restart the timer for sending keep-alive like packets to node(s)
        -- we're subscribed to as when we keep receiving blocks from them it
        -- means the connection is sound.
        slotDuration <- fromIntegral . toMicroseconds <$> getCurrentEpochSlotDuration
        setTimerDuration keepAliveTimer $ 3 * slotDuration
        startTimer keepAliveTimer
        thingToDoNext
        mainLoop
    mainLoopE e = do
        -- REPORT:ERROR 'reportOrLogE' in block retrieval worker.
        reportOrLogE "retrievalWorker mainLoopE: error caught " e
        delay (sec 1)
        mainLoop

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
                updateRecoveryHeader nodeId header

    -----------------

    handleRecoveryWithHandler nodeId header =
        handleAny (handleRecoveryE nodeId header) $
        handleRecovery nodeId header

    -- We immediately drop recovery mode/header and request tips
    -- again.
    handleRecoveryE nodeId header e = do
        -- REPORT:ERROR 'reportOrLogW' in block retrieval worker/recovery.
        reportOrLogW (sformat
            ("handleRecoveryE: error handling nodeId="%build%", header="%build%": ")
            nodeId (headerHash header)) e
        dropUpdateHeader
        dropRecoveryHeaderAndRepeat diffusion nodeId

    -- Recovery handling. We assume that header in the recovery variable is
    -- appropriate and just query headers/blocks.
    handleRecovery nodeId header = do
        logDebug "Block retrieval queue is empty and we're in recovery mode,\
                 \ so we will request more headers and blocks"
        mkHeadersRequest (headerHash header) >>= \case
            MhrrBlockAdopted ->
                -- How did we even got into recovery then?
                throwM $ DialogUnexpected "handleRecovery: got MhrrBlockAdopted"
            MhrrWithCheckpoints mgh -> do
                logDebug "handleRecovery: asking for headers and blocks"
                getProcessBlocks diffusion nodeId header (mghFrom mgh)

----------------------------------------------------------------------------
-- Entering and exiting recovery mode
----------------------------------------------------------------------------

-- | Result of attempt to update recovery header.
data UpdateRecoveryResult ssc
    = RecoveryStarted NodeId BlockHeader
      -- ^ Recovery header was absent, so we've set it.
    | RecoveryShifted NodeId BlockHeader NodeId BlockHeader
      -- ^ Header was present, but we've replaced it with another
      -- (more difficult) one.
    | RecoveryContinued NodeId BlockHeader
      -- ^ Header is good, but is irrelevant, so recovery variable is
      -- unchanged.

-- | Be careful to run this in the same thread that ends recovery mode
-- (or synchronise those threads with an MVar), otherwise a race
-- condition can occur where we are caught in the recovery mode
-- indefinitely.
updateRecoveryHeader
    :: BlockWorkMode ctx m
    => NodeId
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
                         RecoveryShifted oldNodeId oldHdr nodeId hdr
                    else return $ RecoveryContinued oldNodeId oldHdr
    logDebug $ case updated of
        RecoveryStarted rNodeId rHeader -> sformat
            ("Recovery started with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)
        RecoveryShifted rNodeId' rHeader' rNodeId rHeader -> sformat
            ("Recovery shifted from nodeId="%build%" and tip="%build%
             " to nodeId="%build%" and tip="%build)
            rNodeId' (headerHash rHeader')
            rNodeId  (headerHash rHeader)
        RecoveryContinued rNodeId rHeader -> sformat
            ("Recovery continued with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)

dropUpdateHeader :: BlockWorkMode ctx m => m ()
dropUpdateHeader = do
    progressHeaderVar <- view (lensOf @ProgressHeaderTag)
    void $ atomically $ tryTakeTMVar progressHeaderVar

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
    => NodeId
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
    => Diffusion m -> NodeId -> m ()
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
    case result of
      Left getBlocksError -> do
          let msg = sformat ("Error retrieving blocks from "%listJson%
                             " to "%shortHashF%" from peer "%
                             build%": "%shown)
                            checkpoints (headerHash desired) nodeId getBlocksError
          logWarning msg
          throwM $ DialogUnexpected msg
      Right [] -> do
          let msg = sformat ("Error retrieving blocks from "%listJson%
                             " to "%shortHashF%" from peer "%
                             build%": unexpected empty list")
                            checkpoints (headerHash desired) nodeId
          throwM $ DialogUnexpected msg
      Right (headBlocks : tailBlocks) -> do
          let blocks = OldestFirst (headBlocks :| tailBlocks)
          recHeaderVar <- view (lensOf @RecoveryHeaderTag)
          logDebug $ sformat
              ("Retrieved "%int%" blocks of total size "%builder%": "%listJson)
              (blocks ^. _OldestFirst . to NE.length)
              (unitBuilder $ biSize blocks)
              (map (headerHash . view blockHeader) blocks)
          handleBlocks nodeId blocks diffusion 
          dropUpdateHeader
          -- If we've downloaded any block with bigger
          -- difficulty than ncrecoveryheader, we're
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
