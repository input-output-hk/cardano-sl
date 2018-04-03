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

import           Control.Exception.Safe (Exception (..))
import           Control.Lens (to)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, int, sformat, shown, stext, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary.Communication ()
import           Pos.Block.Configuration (recoveryHeadersMessage)
import           Pos.Block.Network (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..))
import           Pos.Communication.Limits (HasAdoptedBlockVersionData, recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, ListenerSpec, MkListeners (..),
                                             MsgType (..), NodeId, Origin (..), OutSpecs,
                                             constantListeners, waitForConversations)
import           Pos.Core (HeaderHash, headerHash, bvdSlotDuration, prevBlockL)
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
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), nonEmptyNewestFirst,
                                  toOldestFirst, _NewestFirst, _OldestFirst)
import           Pos.Util.Timer (Timer, startTimer)
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
       ( MonadThrow m )
    => (t2 -> (t1 -> t -> NonEmpty x) -> m (Map NodeId (m b)))
    -> t2
    -> x
    -> m b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>= waitForConversations
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
       , HasAdoptedBlockVersionData d
       )
    => Logic d
    -> EnqueueMsg d
    -> NodeId
    -> BlockHeader
    -> [HeaderHash]
    -> d (OldestFirst [] Block)
getBlocks logic enqueue nodeId tipHeader checkpoints = do
    -- It is apparently an error to request headers for the tipHeader and
    -- [tipHeader], i.e. 1 checkpoint equal to the header of the block that
    -- you want. Sure, it's a silly thing to do, but should it be an error?
    --
    -- Anyway, the procedure was and still is: if it's just one block you want,
    -- then you can skip requesting the headers and go straight to requesting
    -- the block itself.
    blocks <- if singleBlockHeader
              then requestBlocks (OldestFirst (one tipHeader))
              else requestAndClassifyHeaders >>= requestBlocks
    pure (OldestFirst (reverse (toList blocks)))
  where

    requestAndClassifyHeaders :: d (OldestFirst NE BlockHeader)
    requestAndClassifyHeaders = do
        headers <- toOldestFirst <$> requestHeaders
        getLcaMainChain logic headers >>= \case
            Nothing -> throwM $ DialogUnexpected $ "Got headers, but couldn't compute " <>
                                                   "LCA to ask for blocks"
            Just (lca :: HeaderHash) -> do
                -- Headers list is (oldest to newest)
                -- [n1,n2,...nj,lca,nj+2,...nk] we drop [n1..lca] and
                -- return [nj+2..nk], as we already have lca in our
                -- local db. Usually this function does 1 iterations
                -- as it's a common case that [n1..nj] is absent and
                -- lca is the oldest header.
                let dropUntilLca = NE.dropWhile (\h -> h ^. prevBlockL /= lca)
                case nonEmpty (dropUntilLca $ getOldestFirst headers) of
                    Nothing -> throwM $ DialogUnexpected $
                                   "All headers are older than LCA, nothing to query"
                    Just headersSuffix -> pure (OldestFirst headersSuffix)

    singleBlockHeader :: Bool
    singleBlockHeader = case checkpoints of
        [checkpointHash] -> checkpointHash == tipHash
        _                -> False
    mgh :: MsgGetHeaders
    mgh = MsgGetHeaders
        { mghFrom = checkpoints
        , mghTo = Just tipHash
        }

    tipHash :: HeaderHash
    tipHash = headerHash tipHeader

    -- | Make message which requests chain of blocks which is based on our
    -- tip. LcaChild is the first block after LCA we don't
    -- know. WantedBlock is the newest one we want to get.
    mkBlocksRequest :: HeaderHash -> HeaderHash -> MsgGetBlocks
    mkBlocksRequest lcaChild wantedBlock =
        MsgGetBlocks
        { mgbFrom = lcaChild
        , mgbTo = wantedBlock
        }

    requestHeaders :: d (NewestFirst NE BlockHeader)
    requestHeaders = enqueueMsgSingle
        enqueue
        (MsgRequestBlockHeaders (Just (S.singleton nodeId)))
        (Conversation requestHeadersConversation)

    requestHeadersConversation
        :: ConversationActions MsgGetHeaders MsgHeaders d
        -> d (NewestFirst NE BlockHeader)
    requestHeadersConversation conv = do
        logDebug $ sformat ("requestHeaders: sending "%build) mgh
        send conv mgh
        mHeaders <- recvLimited conv
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

    requestBlocks :: OldestFirst NE BlockHeader -> d (NewestFirst NE Block)
    requestBlocks headers = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation headers)

    requestBlocksConversation
        :: OldestFirst NE BlockHeader
        -> ConversationActions MsgGetBlocks MsgBlock d
        -> d (NewestFirst NE Block)
    requestBlocksConversation headers conv = do
        -- Preserved behaviour from existing logic code: all of the headers
        -- except for the first and last are tossed away.
        -- TODO don't be so wasteful [CSL-2148]
        let oldestHeader = headers ^. _OldestFirst . _neHead
            newestHeader = headers ^. _OldestFirst . _neLast
            numBlocks = length headers
            lcaChild = oldestHeader
            newestHash = headerHash newestHeader
            lcaChildHash = headerHash lcaChild
        logDebug $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChildHash
                           newestHash
        send conv $ mkBlocksRequest lcaChildHash newestHash
        logDebug "Requested blocks, waiting for the response"
        chainE <- runExceptT (retrieveBlocks conv numBlocks)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks from "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
                                  lcaChildHash newestHash nodeId e
                logWarning msg
                throwM $ DialogUnexpected msg
            Right bs -> case nonEmptyNewestFirst bs of
                Nothing -> do
                    let msg = sformat ("Peer gave an empty blocks list")
                    throwM $ DialogUnexpected msg
                Just blocks -> return blocks

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> Int
        -> ExceptT Text d (NewestFirst [] Block)
    retrieveBlocks conv numBlocks = retrieveBlocksDo conv numBlocks []

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
        -> Int        -- ^ Index of block we're requesting
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text d (NewestFirst [] Block)
    retrieveBlocksDo conv !i !acc
        | i <= 0    = pure $ NewestFirst acc
        | otherwise = lift (recvLimited conv) >>= \case
              Nothing ->
                  throwError $ sformat ("Block retrieval cut short by peer at index #"%int) i
              Just (MsgNoBlock t) ->
                  throwError $ sformat ("Peer failed to produce block #"%int%": "%stext) i t
              Just (MsgBlock block) -> do
                  retrieveBlocksDo conv (i - 1) (block : acc)

requestTip
    :: forall d t .
       ( DiffusionWorkMode d
       , HasAdoptedBlockVersionData d
       )
    => EnqueueMsg d
    -> (BlockHeader -> NodeId -> d t)
    -> d (Map NodeId (d t))
requestTip enqueue k = enqueue (MsgRequestBlockHeaders Nothing) $ \nodeId _ -> pure . Conversation $
    \(conv :: ConversationActions MsgGetHeaders MsgHeaders m) -> do
        logDebug "Requesting tip..."
        send conv (MsgGetHeaders [] Nothing)
        received <- recvLimited conv
        case received of
            Just headers -> handleTip nodeId headers
            Nothing      -> throwM $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        k tip nodeId
    handleTip _ t = do
        logWarning $ sformat ("requestTip: got enexpected response: "%shown) t
        throwM $ DialogUnexpected "peer sent more than one tip"

-- | Announce a block header.
announceBlockHeader
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> EnqueueMsg d
    -> MainBlockHeader
    -> d (Map NodeId (d ()))
announceBlockHeader logic enqueue header =  do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
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
        handleHeadersCommunication logic cA

-- | A conversation for incoming MsgGetHeaders messages.
-- For each of these messages, we'll try to send back the relevant headers,
-- until the client closes up.
handleHeadersCommunication
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> ConversationActions MsgHeaders MsgGetHeaders d
    -> d ()
handleHeadersCommunication logic conv = do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
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
        handleHeadersCommunication logic conv
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
       , HasAdoptedBlockVersionData m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer -- ^ Keepalive timer
    -> MkListeners m
blockListeners logic oq keepaliveTimer = constantListeners $
    [ -- Peer wants some block headers from us.
      handleGetHeaders logic oq
      -- Peer wants some blocks from us.
    , handleGetBlocks logic oq
      -- Peer has a block header for us (yes, singular only).
    , handleBlockHeaders logic oq keepaliveTimer
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
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders logic oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication logic conv

-- | Handler for a GetBlocks request from a client.
-- It looks up the Block corresponding to each HeaderHash and sends it.
handleGetBlocks
    :: forall pack m.
       ( DiffusionWorkMode m )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks logic oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv
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
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
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
       , HasAdoptedBlockVersionData m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer
    -> (ListenerSpec m, OutSpecs)
handleBlockHeaders logic oq keepaliveTimer =
  listenerConv @MsgGetHeaders oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    mHeaders <- recvLimited conv
    whenJust mHeaders $ \case
        (MsgHeaders headers) -> do
            -- Reset the keepalive timer.
            slotDuration <- bvdSlotDuration <$> getAdoptedBVData logic
            startTimer (3 * slotDuration) keepaliveTimer
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
