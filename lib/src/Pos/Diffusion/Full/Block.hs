{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Diffusion.Full.Block
    ( getBlocks
    , requestTip
    , announceBlockHeader
    , handleHeadersCommunication

    , blockListeners
    ) where

import           Universum

import qualified Control.Concurrent.STM as Conc
import           Control.Exception.Safe (Exception (..))
import           Control.Lens (to)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text.Buildable as B
import           Data.Time.Units (toMicroseconds, fromMicroseconds)
import           Formatting (bprint, build, int, sformat, shown, stext, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary.Communication ()
import           Pos.Block.Network (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..))
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlMsgGetBlocks, mlMsgHeaders, mlMsgBlock,
                                           mlMsgGetHeaders)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, ListenerSpec, MkListeners (..),
                                             MsgType (..), NodeId, Origin (..), OutSpecs,
                                             constantListeners, waitForConversations,
                                             waitForDequeues, recvLimited)
import           Pos.Core (BlockVersionData, HeaderHash, ProtocolConstants (..), bvdSlotDuration,
                           headerHash, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader (..), MainBlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (DBMalformed))
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)
-- Dubious having this security stuff in here.
import           Pos.Security.Params (AttackTarget (..), AttackType (..), NodeAttackedError (..),
                                      SecurityParams (..))
import           Pos.Util (_neHead, _neLast)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..),
                                  toOldestFirst, _NewestFirst, _OldestFirst)
import           Pos.Util.Timer (Timer, setTimerDuration, startTimer)
import           Pos.Util.TimeWarp (NetworkAddress, nodeIdToAddress)

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
enqueueMsgSingle ::
       ( MonadThrow m, MonadIO m )
    => (t2 -> (t1 -> t -> NonEmpty x) -> m (Map NodeId (Conc.TVar (OQ.PacketStatus b))))
    -> t2
    -> x
    -> m b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>= waitForConversations . waitForDequeues
    case toList results of
        [] ->      throwM $ DialogUnexpected $
            "enqueueMsgSingle: contacted no peers"
        (_:_:_) -> throwM $ DialogUnexpected $
            "enqueueMsgSingle: contacted more than one peers, probably internal error"
        [x] -> pure x

-- | Get some blocks from the network.
-- No verification is done
getBlocks
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> Word -- ^ Historical: limit on how many headers you can get back... always 2200
    -> EnqueueMsg d
    -> NodeId
    -> HeaderHash
    -> [HeaderHash]
    -> d (OldestFirst [] Block)
getBlocks logic recoveryHeadersMessage enqueue nodeId tipHeaderHash checkpoints = do
    -- It is apparently an error to request headers for the tipHeader and
    -- [tipHeader], i.e. 1 checkpoint equal to the header of the block that
    -- you want. Sure, it's a silly thing to do, but should it be an error?
    --
    -- Anyway, the procedure was and still is: if it's just one block you want,
    -- then you can skip requesting the headers and go straight to requesting
    -- the block itself.
    bvd <- getAdoptedBVData logic
    blocks <- if singleBlockHeader
              then requestBlocks bvd (OldestFirst (one tipHeaderHash))
              else requestAndClassifyHeaders bvd >>= requestBlocks bvd . fmap headerHash
    pure (OldestFirst (reverse (toList blocks)))
  where

    requestAndClassifyHeaders :: BlockVersionData -> d (OldestFirst [] BlockHeader)
    requestAndClassifyHeaders bvd = do
        OldestFirst headers <- toOldestFirst <$> requestHeaders bvd
        -- Logic layer gives us the suffix of the chain that we don't have.
        -- Possibly empty.
        -- 'requestHeaders' gives a NonEmpty; we drop it to a [].
        getLcaMainChain logic (OldestFirst (toList headers))

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

    requestHeaders :: BlockVersionData -> d (NewestFirst NE BlockHeader)
    requestHeaders bvd = enqueueMsgSingle
        enqueue
        (MsgRequestBlockHeaders (Just (S.singleton nodeId)))
        (Conversation (requestHeadersConversation bvd))

    requestHeadersConversation
        :: BlockVersionData
        -> ConversationActions MsgGetHeaders MsgHeaders d
        -> d (NewestFirst NE BlockHeader)
    requestHeadersConversation bvd conv = do
        logDebug $ sformat ("requestHeaders: sending "%build) mgh
        send conv mgh
        mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
        inRecovery <- recoveryInProgress logic
        -- TODO: it's very suspicious to see False here as RequestHeaders
        -- is only called when we're in recovery mode.
        logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
        case mHeaders of
            Nothing -> do
                logWarning "requestHeaders: received Nothing as a response on MsgGetHeaders"
                throwM $ DialogUnexpected $
                    sformat ("requestHeaders: received Nothing from "%build) nodeId
            Just (MsgNoHeaders t) -> do
                logWarning $ "requestHeaders: received MsgNoHeaders: " <> t
                throwM $ DialogUnexpected $
                    sformat ("requestHeaders: received MsgNoHeaders from "%
                             build%", msg: "%stext)
                            nodeId
                            t
            Just (MsgHeaders headers) -> do
                logDebug $ sformat
                    ("requestHeaders: received "%int%" headers from nodeId "%build)
                    (headers ^. _NewestFirst . to NE.length)
                    nodeId
                return headers

    requestBlocks :: BlockVersionData -> OldestFirst [] HeaderHash -> d (NewestFirst [] Block)
    requestBlocks _   (OldestFirst [])     = pure (NewestFirst [])
    requestBlocks bvd (OldestFirst (b:bs)) = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation bvd (OldestFirst (b :| bs)))

    requestBlocksConversation
        :: BlockVersionData
        -> OldestFirst NE HeaderHash
        -> ConversationActions MsgGetBlocks MsgBlock d
        -> d (NewestFirst [] Block)
    requestBlocksConversation bvd headers conv = do
        -- Preserved behaviour from existing logic code: all of the headers
        -- except for the first and last are tossed away.
        -- TODO don't be so wasteful [CSL-2148]
        let oldestHeader = headers ^. _OldestFirst . _neHead
            newestHeader = headers ^. _OldestFirst . _neLast
            numBlocks = length headers
            lcaChild = oldestHeader
        logDebug $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChild
                           newestHeader
        send conv $ mkBlocksRequest lcaChild newestHeader
        logDebug "Requested blocks, waiting for the response"
        chainE <- runExceptT (retrieveBlocks conv bvd numBlocks)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks from "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
                                  lcaChild newestHeader nodeId e
                logWarning msg
                throwM $ DialogUnexpected msg
            Right bs -> return bs

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> BlockVersionData
        -> Int
        -> ExceptT Text d (NewestFirst [] Block)
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
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> BlockVersionData
        -> Int        -- ^ Index of block we're requesting
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text d (NewestFirst [] Block)
    retrieveBlocksDo conv bvd !i !acc
        | i <= 0    = pure $ NewestFirst acc
        | otherwise = lift (recvLimited conv (mlMsgBlock bvd)) >>= \case
              Nothing ->
                  throwError $ sformat ("Block retrieval cut short by peer at index #"%int) i
              Just (MsgNoBlock t) ->
                  throwError $ sformat ("Peer failed to produce block #"%int%": "%stext) i t
              Just (MsgBlock block) -> do
                  retrieveBlocksDo conv bvd (i - 1) (block : acc)

requestTip
    :: forall d .
       ( DiffusionWorkMode d )
    => Logic d
    -> EnqueueMsg d
    -> Word
    -> d (Map NodeId (d BlockHeader))
requestTip logic enqueue recoveryHeadersMessage = fmap waitForDequeues $
    enqueue (MsgRequestBlockHeaders Nothing) $ \nodeId _ -> pure . Conversation $
        \(conv :: ConversationActions MsgGetHeaders MsgHeaders m) -> do
            logDebug "Requesting tip..."
            bvd <- getAdoptedBVData logic
            send conv (MsgGetHeaders [] Nothing)
            received <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
            case received of
                Just headers -> handleTip nodeId headers
                Nothing      -> throwM $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%" from "%shown%", processing") (headerHash tip) nodeId
        pure tip
    handleTip _ t = do
        logWarning $ sformat ("requestTip: got enexpected response: "%shown) t
        throwM $ DialogUnexpected "peer sent more than one tip"

-- | Announce a block header.
announceBlockHeader
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> ProtocolConstants
    -> Word
    -> EnqueueMsg d
    -> MainBlockHeader
    -> d (Map NodeId (d ()))
announceBlockHeader logic protocolConstants recoveryHeadersMessage enqueue header =  do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    waitForDequeues <$> enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        -- TODO figure out what this security stuff is doing and judge whether
        -- it needs to change / be removed.
        let sparams = securityParams logic
        -- Copied from Pos.Security.Util but made pure. The existing
        -- implementation was tied to a reader rather than taking a
        -- SecurityParams value as a function argument.
            shouldIgnoreAddress :: NetworkAddress -> Bool
            shouldIgnoreAddress addr = and
                [ AttackNoBlocks `elem` spAttackTypes sparams
                , NetworkAddressTarget addr `elem` spAttackTargets sparams
                ]
            throwOnIgnored :: NodeId -> d ()
            throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    when (shouldIgnoreAddress addr) $
                        throwM AttackNoBlocksTriggered
        -- TODO the when condition is not necessary, as it's a part of the
        -- conjunction in shouldIgnoreAddress
        when (AttackNoBlocks `elem` spAttackTypes sparams) (throwOnIgnored nodeId)
        logDebug $
            sformat
                ("Announcing block"%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (BlockHeaderMain header))
        -- After we announce, the peer is given an opportunity to request more
        -- headers within the same conversation.
        handleHeadersCommunication logic protocolConstants recoveryHeadersMessage cA

-- | A conversation for incoming MsgGetHeaders messages.
-- For each of these messages, we'll try to send back the relevant headers,
-- until the client closes up.
handleHeadersCommunication
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> ProtocolConstants
    -> Word
    -> ConversationActions MsgHeaders MsgGetHeaders d
    -> d ()
handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv = do
    let bc = fromIntegral (pcK protocolConstants)
    whenJustM (recvLimited conv (mlMsgGetHeaders bc)) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
        -- FIXME
        -- Diffusion layer is entirely capable of serving blocks even if the
        -- logic layer is in recovery mode.
        ifM (recoveryInProgress logic) onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                -- This is how a peer requests our tip: empty checkpoint list,
                -- Nothing for the limiting hash.
                ([], Nothing) -> Right . one <$> getLastMainHeader
                -- This is how a peer requests one particular header: empty
                -- checkpoint list, Just for the limiting hash.
                ([], Just h)  -> do
                    mHeader <- getBlockHeader logic h
                    pure . maybeToRight "getBlockHeader returned Nothing" . fmap one $ mHeader
                -- This is how a peer requests a chain of headers.
                -- NB: if the limiting hash is Nothing, getBlockHeaders will
                -- substitute our current tip.
                (c1:cxs, _)   ->
                    first show <$>
                    getBlockHeaders logic (Just recoveryHeadersMessage) (c1:|cxs) mghTo
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: d BlockHeader
    getLastMainHeader = do
        tip :: Block <- getTip logic
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> do
                mHeader <- getBlockHeader logic (tip ^. prevBlockL)
                pure $ fromMaybe tipHeader mHeader
            Right _ -> pure tipHeader
    handleSuccess :: NewestFirst NE BlockHeader -> d ()
    handleSuccess h = do
        send conv (MsgHeaders h)
        logDebug "handleGetHeaders: responded successfully"
        handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv
    onNoHeaders reason = do
        let err = "getheadersFromManyTo returned Nothing, reason: " <> reason
        logWarning err
        send conv (MsgNoHeaders err)
    onRecovery = do
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
        send conv (MsgNoHeaders "server node is in recovery mode")


-- |
-- = Listeners

-- | All block-related listeners.
blockListeners
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer -- ^ Keepalive timer
    -> MkListeners m
blockListeners logic protocolConstants recoveryHeadersMessage oq keepaliveTimer = constantListeners $
    [ -- Peer wants some block headers from us.
      handleGetHeaders logic protocolConstants recoveryHeadersMessage oq
      -- Peer wants some blocks from us.
    , handleGetBlocks logic recoveryHeadersMessage oq
      -- Peer has a block header for us (yes, singular only).
    , handleBlockHeaders logic oq recoveryHeadersMessage keepaliveTimer
    ]

----------------------------------------------------------------------------
-- Getters (return currently stored data)
----------------------------------------------------------------------------

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall pack m.
       ( DiffusionWorkMode m )
    => Logic m
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders logic protocolConstants recoveryHeadersMessage oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv

-- | Handler for a GetBlocks request from a client.
-- It looks up the Block corresponding to each HeaderHash and sends it.
handleGetBlocks
    :: forall pack m.
       ( DiffusionWorkMode m )
    => Logic m
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks logic recoveryHeadersMessage oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv mlMsgGetBlocks
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
        -- [CSL-2148] will probably make this a faster, streaming style:
        -- get the blocks directly from headers rather than getting the list
        -- of headers, then one-by-one getting the corresponding blocks.
        -- As such, the DBMalformed error below (failMalformed) won't be
        -- necessary: the streaming thing (probably a conduit) can determine
        -- whether the DB is malformed. Really, this listener has no business
        -- deciding that the database is malformed.
        hashesM <- getHashesRange logic (Just recoveryHeadersMessage) mgbFrom mgbTo
        case hashesM of
            Right hashes -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one")
                    (length hashes) nodeId 
                for_ hashes $ \hHash ->
                    getBlock logic hHash >>= \case
                        Just b -> send conv $ MsgBlock b
                        Nothing  -> do
                            send conv $ MsgNoBlock ("Couldn't retrieve block with hash " <>
                                                    pretty hHash)
                            failMalformed
                logDebug "handleGetBlocks: blocks sending done"
            Left e -> logWarning $ "getBlocksByHeaders@retrieveHeaders returned error: " <> show e
  where
    -- See note above in the definition of handleGetBlocks [CSL-2148].
    failMalformed =
        throwM $ DBMalformed $
        "handleGetBlocks: getHashesRange returned header that doesn't " <>
        "have corresponding block in storage."

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall pack m.
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> Word
    -> Timer
    -> (ListenerSpec m, OutSpecs)
handleBlockHeaders logic oq recoveryHeadersMessage keepaliveTimer =
  listenerConv @MsgGetHeaders oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    bvd <- getAdoptedBVData logic
    mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
    whenJust mHeaders $ \case
        (MsgHeaders headers) -> do
            -- Reset the keepalive timer.
            slotDuration <- toMicroseconds . bvdSlotDuration <$> getAdoptedBVData logic
            setTimerDuration keepaliveTimer $ fromMicroseconds (3 * slotDuration)
            startTimer keepaliveTimer
            handleUnsolicitedHeaders logic (getNewestFirst headers) nodeId
        _ -> pass -- Why would somebody propagate 'MsgNoHeaders'? We don't care.

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: ( DiffusionWorkMode m )
    => Logic m
    -> NonEmpty BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeaders logic (header :| []) nodeId =
    postBlockHeader logic header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders _ (h:|hs) _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)
