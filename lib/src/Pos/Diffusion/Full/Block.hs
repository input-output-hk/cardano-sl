{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Diffusion.Full.Block
    ( getBlocks
    , requestTip
    , announceBlockHeader
    , handleHeadersCommunication
    , streamBlocks
    , blockListeners
    ) where

import           Universum

import           Control.Concurrent.Async (cancel)
import qualified Control.Concurrent.STM as Conc
import           Control.Exception (Exception (..), throwIO)
import           Control.Lens (to)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit (await, runConduit, (.|))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import           Formatting (bprint, build, int, sformat, shown, stext, (%))
import qualified Formatting.Buildable as B
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Conversation (sendRaw)
import           Serokell.Util.Text (listJson)
import qualified System.Metrics.Gauge as Gauge

import           Pos.Binary.Communication (serializeMsgSerializedBlock,
                     serializeMsgStreamBlock)
import           Pos.Communication.Limits (mlMsgBlock, mlMsgGetBlocks,
                     mlMsgGetHeaders, mlMsgHeaders, mlMsgStream,
                     mlMsgStreamBlock)
import           Pos.Core (ProtocolConstants (..), difficultyL)
import           Pos.Core.Block (Block, BlockHeader (..), HeaderHash,
                     MainBlockHeader, blockHeader, headerHash, prevBlockL)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..),
                     toOldestFirst, _NewestFirst, _OldestFirst)
import           Pos.Core.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Core.Update (BlockVersionData, bvdSlotDuration)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (DBMalformed))
import           Pos.Infra.Communication.Listener (listenerConv)
import           Pos.Infra.Communication.Protocol (Conversation (..),
                     ConversationActions (..), EnqueueMsg, ListenerSpec,
                     MkListeners (..), MsgType (..), NodeId, Origin (..),
                     OutSpecs, constantListeners, recvLimited,
                     waitForConversations, waitForDequeues)
import           Pos.Infra.Diffusion.Types (DiffusionHealth (..))
import           Pos.Infra.Network.Types (Bucket)
import           Pos.Infra.Util.TimeWarp (nodeIdToAddress)
import           Pos.Logic.Types (Logic)
import qualified Pos.Logic.Types as Logic
import           Pos.Network.Block.Types (MsgBlock (..), MsgGetBlocks (..),
                     MsgGetHeaders (..), MsgHeaders (..),
                     MsgSerializedBlock (..), MsgStream (..),
                     MsgStreamBlock (..), MsgStreamStart (..),
                     MsgStreamUpdate (..))
-- Dubious having this security stuff in here.
import           Pos.Chain.Security (AttackTarget (..), AttackType (..),
                     NodeAttackedError (..), SecurityParams (..))
import           Pos.Util (_neHead, _neLast)
import           Pos.Util.Timer (Timer, startTimer)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logWarning)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data BlockNetLogicException
    = DialogUnexpected Text
      -- ^ Node's response in any network/block related logic was
      -- unexpected.
    | BlockNetLogicInternal Text
      -- ^ We don't expect this to happen. Most probably it's internal
      -- logic error.
    deriving (Show)

instance B.Buildable BlockNetLogicException where
    build e = bprint ("BlockNetLogicException: "%shown) e

instance Exception BlockNetLogicException where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

----------------------------------------------------------------------------
-- Networking
----------------------------------------------------------------------------

-- | Expects sending message to exactly one node. Receives result or
-- fails if no result was obtained (no nodes available, timeout, etc).
enqueueMsgSingle
    :: (t2 -> (t1 -> t -> NonEmpty x) -> IO (Map NodeId (Conc.TVar (OQ.PacketStatus b))))
    -> t2
    -> x
    -> IO b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>= waitForConversations . waitForDequeues
    case toList results of
        [] ->      liftIO $ throwIO $ DialogUnexpected $
            "enqueueMsgSingle: contacted no peers"
        (_:_:_) -> liftIO $ throwIO $ DialogUnexpected $
            "enqueueMsgSingle: contacted more than one peers, probably internal error"
        [x] -> pure x

-- | Get some blocks from the network.
-- No verification is done
getBlocks
    :: TraceNamed IO
    -> Logic IO
    -> Word -- ^ Historical: limit on how many headers you can get back... always 2200
    -> EnqueueMsg
    -> NodeId
    -> HeaderHash
    -> [HeaderHash]
    -> IO (OldestFirst [] Block)
getBlocks logTrace logic recoveryHeadersMessage enqueue nodeId tipHeaderHash checkpoints = do
    -- It is apparently an error to request headers for the tipHeader and
    -- [tipHeader], i.e. 1 checkpoint equal to the header of the block that
    -- you want. Sure, it's a silly thing to do, but should it be an error?
    --
    -- Anyway, the procedure was and still is: if it's just one block you want,
    -- then you can skip requesting the headers and go straight to requesting
    -- the block itself.
    bvd <- Logic.getAdoptedBVData logic
    blocks <- if singleBlockHeader
              then requestBlocks bvd (OldestFirst (one tipHeaderHash))
              else requestAndClassifyHeaders bvd >>= requestBlocks bvd
    pure (OldestFirst (reverse (toList blocks)))
  where

    requestAndClassifyHeaders :: BlockVersionData -> IO (OldestFirst [] HeaderHash)
    requestAndClassifyHeaders bvd = do
        OldestFirst headers <- toOldestFirst <$> requestHeaders bvd
        -- Logic layer gives us the suffix of the chain that we don't have.
        -- Possibly empty.
        -- 'requestHeaders' gives a NonEmpty; we drop it to a [].
        fmap snd (Logic.getLcaMainChain logic (OldestFirst (toList (fmap headerHash headers))))

    singleBlockHeader :: Bool
    singleBlockHeader = case checkpoints of
        [checkpointHash] -> checkpointHash == tipHeaderHash
        _                -> False
    mgh :: MsgGetHeaders
    mgh = MsgGetHeaders
        { mghFrom = checkpoints
        , mghTo = Just tipHeaderHash
        }

    -- | Make message which requests chain of blocks which is based on our
    -- tip. LcaChild is the first block after LCA we don't
    -- know. WantedBlock is the newest one we want to get.
    mkBlocksRequest :: HeaderHash -> HeaderHash -> MsgGetBlocks
    mkBlocksRequest lcaChild wantedBlock =
        MsgGetBlocks
        { mgbFrom = lcaChild
        , mgbTo = wantedBlock
        }

    requestHeaders :: BlockVersionData -> IO (NewestFirst NE BlockHeader)
    requestHeaders bvd = enqueueMsgSingle
        enqueue
        (MsgRequestBlockHeaders (Just (S.singleton nodeId)))
        (Conversation (requestHeadersConversation bvd))

    requestHeadersConversation
        :: BlockVersionData
        -> ConversationActions MsgGetHeaders MsgHeaders
        -> IO (NewestFirst NE BlockHeader)
    requestHeadersConversation bvd conv = do
        logDebug logTrace $ sformat ("requestHeaders: sending "%build) mgh
        send conv mgh
        mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
        inRecovery <- Logic.recoveryInProgress logic
        -- TODO: it's very suspicious to see False here as RequestHeaders
        -- is only called when we're in recovery mode.
        logDebug logTrace $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
        case mHeaders of
            Nothing -> do
                logWarning logTrace $ "requestHeaders: received Nothing as a response on MsgGetHeaders"
                throwIO $ DialogUnexpected $
                    sformat ("requestHeaders: received Nothing from "%build) nodeId
            Just (MsgNoHeaders t) -> do
                logWarning logTrace $ "requestHeaders: received MsgNoHeaders: " <> t
                throwIO $ DialogUnexpected $
                    sformat ("requestHeaders: received MsgNoHeaders from "%
                             build%", msg: "%stext)
                            nodeId
                            t
            Just (MsgHeaders headers) -> do
                logDebug logTrace $ sformat
                    ("requestHeaders: received "%int%" headers from nodeId "%build)
                    (headers ^. _NewestFirst . to NE.length)
                    nodeId
                return headers

    requestBlocks :: BlockVersionData -> OldestFirst [] HeaderHash -> IO (NewestFirst [] Block)
    requestBlocks _   (OldestFirst [])     = pure (NewestFirst [])
    requestBlocks bvd (OldestFirst (b:bs)) = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation bvd (OldestFirst (b :| bs)))

    requestBlocksConversation
        :: BlockVersionData
        -> OldestFirst NE HeaderHash
        -> ConversationActions MsgGetBlocks MsgBlock
        -> IO (NewestFirst [] Block)
    requestBlocksConversation bvd headers conv = do
        -- Preserved behaviour from existing logic code: all of the headers
        -- except for the first and last are tossed away.
        -- TODO don't be so wasteful [CSL-2148]
        let oldestHeader = headers ^. _OldestFirst . _neHead
            newestHeader = headers ^. _OldestFirst . _neLast
            numBlocks = length headers
            lcaChild = oldestHeader
        logDebug logTrace $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChild
                           newestHeader
        send conv $ mkBlocksRequest lcaChild newestHeader
        logDebug logTrace $ "Requested blocks, waiting for the response"
        chainE <- runExceptT (retrieveBlocks conv bvd numBlocks)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks from "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
                                  lcaChild newestHeader nodeId e
                logWarning logTrace msg
                throwIO $ DialogUnexpected msg
            Right bs -> return bs

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
        :: ConversationActions MsgGetBlocks MsgBlock
        -> BlockVersionData
        -> Int
        -> ExceptT Text IO (NewestFirst [] Block)
    retrieveBlocks conv bvd numBlocks = retrieveBlocksDo conv bvd numBlocks []

    -- Content of retrieveBlocks.
    -- Receive a given number of blocks. If the server doesn't send this
    -- many blocks, an error will be given.
    --
    -- Copied from the old logic but modified to use an accumulator rather
    -- than fmapping (<|). That changed the order so we're now NewestFirst
    -- (presumably the server sends them oldest first, as that assumption was
    -- required for the old version to correctly say OldestFirst).
    retrieveBlocksDo
        :: ConversationActions MsgGetBlocks MsgBlock
        -> BlockVersionData
        -> Int        -- ^ Index of block we're requesting
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text IO (NewestFirst [] Block)
    retrieveBlocksDo conv bvd !i !acc
        | i <= 0    = pure $ NewestFirst acc
        | otherwise = lift (recvLimited conv (mlMsgBlock bvd)) >>= \case
              Nothing ->
                  throwError $ sformat ("Block retrieval cut short by peer at index #"%int) i
              Just (MsgNoBlock t) ->
                  throwError $ sformat ("Peer failed to produce block #"%int%": "%stext) i t
              Just (MsgBlock block) -> do
                  retrieveBlocksDo conv bvd (i - 1) (block : acc)

-- | Datatype used for the queue of blocks, produced by network streaming and
-- then consumed by a continuation resonsible for writing blocks to store.
-- StreamEnd signals end of stream.
data StreamEntry = StreamEnd | StreamBlock !Block

-- | Stream some blocks from the network.
-- Returns Nothing if streaming is disabled by the client or not supported by the peer.
streamBlocks
    :: forall t .
       TraceNamed IO
    -> Maybe DiffusionHealth
    -> Logic IO
    -> Word32
    -> EnqueueMsg
    -> NodeId
    -> HeaderHash
    -> [HeaderHash]
    -> ([Block] -> IO t)
    -> IO (Maybe t)
streamBlocks _        _   _     0            _       _      _         _           _ = return Nothing -- Fallback to batch mode
streamBlocks logTrace smM logic streamWindow enqueue nodeId tipHeader checkpoints k = do
    blockChan <- atomically $ Conc.newTBQueue $ fromIntegral streamWindow
    let batchSize = min 64 streamWindow
    fallBack <- atomically $ Conc.newTVar False
    requestVar <- requestBlocks fallBack blockChan
    r <- processBlocks batchSize 0 [] blockChan `finally` (atomically $ do
        status <- Conc.readTVar requestVar
        case status of
             OQ.PacketAborted -> pure (pure ())
             OQ.PacketEnqueued -> do
                 Conc.writeTVar requestVar OQ.PacketAborted
                 pure (pure ())
             OQ.PacketDequeued asyncIO -> pure (cancel asyncIO))
    r' <- atomically $ Conc.readTVar fallBack
    if r' then pure Nothing
          else pure $ Just r
  where

    processBlocks :: Word32 -> Word32 -> [Block] -> Conc.TBQueue StreamEntry -> IO t
    processBlocks batchSize !n !blocks blockChan = do
        streamEntry <- atomically $ Conc.readTBQueue blockChan
        case streamEntry of
             StreamEnd         -> k blocks
             StreamBlock block -> do
                 let n' = n + 1
                 when (n' `mod` 256 == 0) $
                      logDebug logTrace $
                           sformat ("Read block "%shortHashF%" difficulty "%int) (headerHash block)
                                   (block ^. difficultyL)
                 case smM of
                      Nothing -> pure ()
                      Just sm -> liftIO $ Gauge.dec $ dhStreamWriteQueue sm

                 if n' `mod` batchSize == 0
                     then do
                         _ <- k (block : blocks)
                         processBlocks batchSize n' [] blockChan
                     else
                         processBlocks batchSize n' (block : blocks) blockChan

    writeStreamEnd :: Conc.TBQueue StreamEntry -> IO ()
    writeStreamEnd blockChan = atomically $ Conc.writeTBQueue blockChan StreamEnd

    mkStreamStart :: [HeaderHash] -> HeaderHash -> MsgStream
    mkStreamStart chain wantedBlock =
        MsgStart $ MsgStreamStart
        { mssFrom = chain
        , mssTo = wantedBlock
        , mssWindow = streamWindow
        }

    requestBlocks :: Conc.TVar Bool -> Conc.TBQueue StreamEntry -> IO (Conc.TVar (OQ.PacketStatus ()))
    requestBlocks fallBack blockChan = do
        convMap <- enqueue (MsgRequestBlocks (S.singleton nodeId))
                            (\_ _ -> (Conversation $ \it -> requestBlocksConversation blockChan it `finally` writeStreamEnd blockChan) :|
                                      [(Conversation $ \it -> requestBatch fallBack blockChan it `finally` writeStreamEnd blockChan)]
                                     )
        case M.lookup nodeId convMap of
            Just tvar -> pure tvar
            -- FIXME shouldn't have to deal with this.
            -- One possible solution: do the block request in response to an
            -- unsolicited header, so that's it's all done in one conversation,
            -- and so there's no need to even track the 'nodeId'.
            Nothing   -> throwM $ DialogUnexpected $ "requestBlocks did not contact given peer"

    requestBatch
        :: Conc.TVar Bool
        -> Conc.TBQueue StreamEntry
        -> ConversationActions MsgGetBlocks MsgBlock
        -> IO ()
    requestBatch fallBack _ _ = do
        -- The peer doesn't support streaming, we need to fall back to batching but
        -- the current conversation is unusable since there is no way for us to learn
        -- which blocks we shall fetch.
        atomically $ writeTVar fallBack True
        return ()

    requestBlocksConversation
        :: Conc.TBQueue StreamEntry
        -> ConversationActions MsgStream MsgStreamBlock
        -> IO ()
    requestBlocksConversation blockChan conv = do
        let newestHash = headerHash tipHeader
        logDebug logTrace $
            sformat ("streamBlocks: Requesting stream of blocks from "%listJson%" to "%shortHashF)
                     checkpoints
                     newestHash
        send conv $ mkStreamStart checkpoints newestHash
        bvd <- Logic.getAdoptedBVData logic
        retrieveBlocks bvd blockChan conv streamWindow
        return ()

    halfStreamWindow = max 1 $ streamWindow `div` 2

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
        :: BlockVersionData
        -> Conc.TBQueue StreamEntry
        -> ConversationActions MsgStream MsgStreamBlock
        -> Word32
        -> IO ()
    retrieveBlocks bvd blockChan conv window = do
        window' <- if window < halfStreamWindow
            then do
                let w' = streamWindow
                logDebug logTrace $ sformat ("Updating Window: "%int%" to "%int) window w'
                send conv $ MsgUpdate $ MsgStreamUpdate $ w'
                return (w' - 1)
            else return $ window - 1
        block <- retrieveBlock bvd conv
        case block of
             MsgStreamNoBlock t -> do
                 let msg = sformat ("MsgStreamNoBlock "%stext) t
                 logWarning logTrace msg
                 throwM $ DialogUnexpected msg
             MsgStreamEnd -> do
                 logDebug logTrace $ sformat ("Streaming done client-side for node"%build) nodeId
                 return ()
             MsgStreamBlock b -> do
                 -- logDebug logTrace $ sformat ("Read block "%shortHashF) (headerHash b)
                 atomically $ Conc.writeTBQueue blockChan (StreamBlock b)
                 case smM of
                      Nothing -> pure ()
                      Just sm -> do
                          Gauge.inc $ dhStreamWriteQueue sm
                          Gauge.set (dhStreamWindow sm) (fromIntegral window')
                 retrieveBlocks bvd blockChan conv window'

    retrieveBlock
        :: BlockVersionData
        -> ConversationActions MsgStream MsgStreamBlock
        -> IO MsgStreamBlock
    retrieveBlock bvd conv = do
        blockE <- recvLimited conv (mlMsgStreamBlock bvd)
        case blockE of
            Nothing -> do
                let msg = sformat ("Error retrieving blocks from peer "%build) nodeId
                logWarning logTrace msg
                throwM $ DialogUnexpected msg
            Just block -> return block

requestTip
    :: TraceNamed IO
    -> Logic IO
    -> EnqueueMsg
    -> Word
    -> IO (Map NodeId (IO BlockHeader))
requestTip logTrace logic enqueue recoveryHeadersMessage = fmap waitForDequeues $
    enqueue (MsgRequestBlockHeaders Nothing) $ \nodeId _ -> pure . Conversation $
        \(conv :: ConversationActions MsgGetHeaders MsgHeaders) -> do
            logDebug logTrace "Requesting tip..."
            bvd <- Logic.getAdoptedBVData logic
            send conv (MsgGetHeaders [] Nothing)
            received <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
            case received of
                Just headers -> handleTip nodeId headers
                Nothing      -> throwIO $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug logTrace $ sformat ("Got tip "%shortHashF%" from "%shown%", processing") (headerHash tip) nodeId
        pure tip
    handleTip _ t = do
        logWarning logTrace $ sformat ("requestTip: got enexpected response: "%shown) t
        throwIO $ DialogUnexpected "peer sent more than one tip"

-- | Announce a block header.
announceBlockHeader
    :: TraceNamed IO
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> EnqueueMsg
    -> MainBlockHeader
    -> IO (Map NodeId (IO ()))
announceBlockHeader logTrace logic protocolConstants recoveryHeadersMessage enqueue header =  do
    logDebug logTrace $ sformat ("Announcing header to others:\n"%build) header
    waitForDequeues <$> enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        -- TODO figure out what this security stuff is doing and judge whether
        -- it needs to change / be removed.
        let sparams = Logic.securityParams logic
        -- Copied from Pos.Chain.Security but made pure. The existing
        -- implementation was tied to a reader rather than taking a
        -- SecurityParams value as a function argument.
            shouldIgnoreAddress :: NetworkAddress -> Bool
            shouldIgnoreAddress addr = and
                [ AttackNoBlocks `elem` spAttackTypes sparams
                , NetworkAddressTarget addr `elem` spAttackTargets sparams
                ]
            throwOnIgnored :: NodeId -> IO ()
            throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    when (shouldIgnoreAddress addr) $
                        throwIO AttackNoBlocksTriggered
        -- TODO the when condition is not necessary, as it's a part of the
        -- conjunction in shouldIgnoreAddress
        when (AttackNoBlocks `elem` spAttackTypes sparams) (throwOnIgnored nodeId)
        logDebug logTrace $
            sformat
                ("Announcing block"%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (BlockHeaderMain header))
        -- After we announce, the peer is given an opportunity to request more
        -- headers within the same conversation.
        handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage cA

-- | A conversation for incoming MsgGetHeaders messages.
-- For each of these messages, we'll try to send back the relevant headers,
-- until the client closes up.
handleHeadersCommunication
    :: TraceNamed IO
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> ConversationActions MsgHeaders MsgGetHeaders
    -> IO ()
handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv = do
    let bc = fromIntegral (pcK protocolConstants)
    whenJustM (recvLimited conv (mlMsgGetHeaders bc)) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug logTrace $ sformat ("Got request on handleGetHeaders: "%build) mgh
        -- FIXME
        -- Diffusion layer is entirely capable of serving blocks even if the
        -- logic layer is in recovery mode.
        ifM (Logic.recoveryInProgress logic) onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                -- This is how a peer requests our tip: empty checkpoint list,
                -- Nothing for the limiting hash.
                ([], Nothing) -> Right . one <$> getLastMainHeader
                -- This is how a peer requests one particular header: empty
                -- checkpoint list, Just for the limiting hash.
                ([], Just h)  -> do
                    mHeader <- Logic.getBlockHeader logic h
                    pure . maybeToRight "getBlockHeader returned Nothing" . fmap one $ mHeader
                -- This is how a peer requests a chain of headers.
                -- NB: if the limiting hash is Nothing, getBlockHeaders will
                -- substitute our current tip.
                (c1:cxs, _)   ->
                    first show <$>
                    Logic.getBlockHeaders logic (Just recoveryHeadersMessage) (c1:|cxs) mghTo
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: IO BlockHeader
    getLastMainHeader = do
        tip :: Block <- Logic.getTip logic
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> do
                mHeader <- Logic.getBlockHeader logic (tip ^. prevBlockL)
                pure $ fromMaybe tipHeader mHeader
            Right _ -> pure tipHeader
    handleSuccess :: NewestFirst NE BlockHeader -> IO ()
    handleSuccess h = do
        send conv (MsgHeaders h)
        logDebug logTrace "handleGetHeaders: responded successfully"
        handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv
    onNoHeaders reason = do
        let err = "getheadersFromManyTo returned Nothing, reason: " <> reason
        logWarning logTrace err
        send conv (MsgNoHeaders err)
    onRecovery = do
        logDebug logTrace "handleGetHeaders: not responding, we're in recovery mode"
        send conv (MsgNoHeaders "server node is in recovery mode")


-- |
-- = Listeners

-- | All block-related listeners.
blockListeners
    :: TraceNamed IO
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer -- ^ Keepalive timer
    -> MkListeners
blockListeners logTrace logic protocolConstants recoveryHeadersMessage oq keepaliveTimer = constantListeners $
    [ -- Peer wants some block headers from us.
      handleGetHeaders logTrace logic protocolConstants recoveryHeadersMessage oq
      -- Peer wants some blocks from us.
    , handleGetBlocks logTrace logic recoveryHeadersMessage oq
      -- Peer has a block header for us (yes, singular only).
    , handleBlockHeaders logTrace logic oq recoveryHeadersMessage keepaliveTimer
    , handleStreamStart logTrace logic oq
    ]

----------------------------------------------------------------------------
-- Getters (return currently stored data)
----------------------------------------------------------------------------

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall pack.
       TraceNamed IO
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec, OutSpecs)
handleGetHeaders logTrace logic protocolConstants recoveryHeadersMessage oq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv -> do
    logDebug logTrace $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv

-- | Handler for a GetBlocks request from a client.
-- It looks up the Block corresponding to each HeaderHash and sends it.
handleGetBlocks
    :: forall pack.
       TraceNamed IO
    -> Logic IO
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec, OutSpecs)
handleGetBlocks logTrace logic recoveryHeadersMessage oq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv mlMsgGetBlocks
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug logTrace $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
        -- [CSL-2148] will probably make this a faster, streaming style:
        -- get the blocks directly from headers rather than getting the list
        -- of headers, then one-by-one getting the corresponding blocks.
        -- As such, the DBMalformed error below (failMalformed) won't be
        -- necessary: the streaming thing (probably a conduit) can determine
        -- whether the DB is malformed. Really, this listener has no business
        -- deciding that the database is malformed.
        hashesM <- Logic.getHashesRange logic (Just recoveryHeadersMessage) mgbFrom mgbTo
        case hashesM of
            Right hashes -> do
                logDebug logTrace $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one")
                    (length hashes) nodeId
                for_ hashes $ \hHash ->
                    Logic.getSerializedBlock logic hHash >>= \case
                        -- TODO: we should get lazy bytestring from the db layer to
                        -- stream the bytes to the network
                        Just b -> sendRaw conv $ BSL.fromStrict $ serializeMsgSerializedBlock $ MsgSerializedBlock b
                        Nothing  -> do
                            send conv $ MsgNoBlock ("Couldn't retrieve block with hash " <>
                                                    pretty hHash)
                            failMalformed
                logDebug logTrace "handleGetBlocks: blocks sending done"
            Left e -> logWarning logTrace $ "getBlocksByHeaders@retrieveHeaders returned error: " <> show e
  where
    -- See note above in the definition of handleGetBlocks [CSL-2148].
    failMalformed =
        throwIO $ DBMalformed $
        "handleGetBlocks: getHashesRange returned header that doesn't " <>
        "have corresponding block in storage."

handleStreamStart
    :: forall pack.
       TraceNamed IO
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec, OutSpecs)
handleStreamStart logTrace logic oq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv -> do
    msMsg <- recvLimited conv mlMsgStream
    whenJust msMsg $ \ms -> do
        case ms of
             MsgStart s -> do
                 logDebug logTrace $ sformat ("Streaming Request from node "%build) nodeId
                 stream nodeId conv (mssFrom s) (mssTo s) (mssWindow s)
             MsgUpdate _ -> do
                 send conv $ MsgStreamNoBlock "MsgUpdate without MsgStreamStart"
                 logDebug logTrace $ sformat ("MsgStream without MsgStreamStart from node "%build) nodeId
                 return ()

  where
    stream nodeId conv [] _ _ = do
        send conv $ MsgStreamNoBlock "MsgStreamStart with empty from chain"
        logDebug logTrace $ sformat ("MsgStreamStart with empty from chain from node "%build) nodeId
        return ()
    stream nodeId conv (cl:cxs) _ window = do
        -- Find the newest checkpoint which is in our chain (checkpoints are
        -- oldest first).
        (prefix, _) <- Logic.getLcaMainChain logic (OldestFirst (fmap headerHash (cl:cxs)))
        case getNewestFirst prefix of
             [] -> do
                send conv $ MsgStreamNoBlock "handleStreamStart:strean Failed to find lca"
                logDebug logTrace $ sformat ("handleStreamStart:stream getBlockHeaders from "%shown%" failed for "%listJson) nodeId (cl:cxs)
                return ()
             -- 'lca' is the newest client-supplied checkpoint that we have.
             -- We need to begin streaming from its child, which is what
             -- 'Logic.streamBlocks' does.
             lca : _ -> do
                let producer = do
                        Logic.streamBlocks logic lca
                        lift $ send conv MsgStreamEnd
                    consumer = loop nodeId conv window
                runConduit $ producer .| consumer

    loop nodeId conv 0 = do
        lift $ logDebug logTrace $ "handleStreamStart:loop waiting on window update"
        msMsg <- lift $ recvLimited conv mlMsgStream
        whenJust msMsg $ \ms -> do
             case ms of
                  MsgStart _ -> do
                      lift $ send conv $ MsgStreamNoBlock ("MsgStreamStart, expected MsgStreamUpdate")
                      lift $ logDebug logTrace $ sformat ("handleStreamStart:loop MsgStart, expected MsgStreamUpdate from "%build) nodeId
                      return ()
                  MsgUpdate u -> do
                      lift $ logDebug logTrace $ sformat ("handleStreamStart:loop new window "%shown%" from "%build) u nodeId
                      loop nodeId conv (msuWindow u)
    loop nodeId conv window =
        whenJustM await $ \b -> do
            lift $ sendRaw conv $ serializeMsgStreamBlock $ MsgSerializedBlock b
            loop nodeId conv (window - 1)

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall pack .
       TraceNamed IO
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> Word
    -> Timer
    -> (ListenerSpec, OutSpecs)
handleBlockHeaders logTrace logic oq recoveryHeadersMessage keepaliveTimer =
  listenerConv @MsgGetHeaders logTrace oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug logTrace $ "handleBlockHeaders: got some unsolicited block header(s)"
    bvd <- Logic.getAdoptedBVData logic
    mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
    whenJust mHeaders $ \case
        (MsgHeaders headers) -> do
            -- Reset the keepalive timer.
            slotDuration <- bvdSlotDuration <$> Logic.getAdoptedBVData logic
            startTimer (3 * slotDuration) keepaliveTimer
            handleUnsolicitedHeaders logTrace logic (getNewestFirst headers) nodeId
        _ -> pass -- Why would somebody propagate 'MsgNoHeaders'? We don't care.

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: TraceNamed IO
    -> Logic IO
    -> NonEmpty BlockHeader
    -> NodeId
    -> IO ()
handleUnsolicitedHeaders _ logic (header :| []) nodeId =
    Logic.postBlockHeader logic header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders logTrace _ (h:|hs) _ = do
    logWarning logTrace $ "Someone sent us nonzero amount of headers we didn't expect"
    logWarning logTrace $ sformat ("Here they are: "%listJson) (h:hs)
