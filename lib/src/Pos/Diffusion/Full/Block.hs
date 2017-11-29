{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}

module Pos.Diffusion.Full.Block
    ( getBlocks
    , requestTip
    , announceBlock
    , handleHeadersCommunication

    , blockListeners
    ) where

import           Universum

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Exception (Exception (..))
import           Control.Lens (to)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text.Buildable as B
-- TODO hopefully we can get rid of this import. It's needed for the
-- security workers stuff and peeking into some reader context which contains
-- it (part of WorkMode).
import           Formatting (build, sformat, (%), shown, bprint, stext, int, builder)
import           Mockable (throw)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Data.Memory.Units (unitBuilder)
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary.Class (biSize)
-- TODO move Pos.Block.Network.Types to Pos.Diffusion hierarchy.
-- Logic layer won't know of it.
import           Pos.Block.Network.Types (MsgGetHeaders (..), MsgHeaders (..), MsgGetBlocks (..), MsgBlock (..))
import           Pos.Communication.Limits (HasBlockLimits, recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, MsgType (..), NodeId, Origin (..),
                                             waitForConversations, OutSpecs, ListenerSpec,
                                             MkListeners (..), constantListeners)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core (HeaderHash, headerHash, prevBlockL, headerHashG)
import           Pos.Core.Block (Block, BlockHeader, MainBlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.Diffusion.Types (GetBlocksError (..))
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)
import           Pos.Logic.Types (Logic (..), GetBlockHeadersError (..), GetTipError (..))
import           Pos.Network.Types (Bucket)
-- Dubious having this security stuff in here.
-- NB: the logic-layer security policy is actually available here in the
-- diffusion layer by way of the WorkMode constraint.
import           Pos.Security (AttackType (..), NodeAttackedError (..),
                               AttackTarget (..), SecurityParams (..))
import           Pos.Util (_neHead, _neLast)
import           Pos.Util.Chrono (NewestFirst (..), _NewestFirst, NE)
import           Pos.Util.TimeWarp (nodeIdToAddress, NetworkAddress)

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


-- | Result of 'matchHeadersRequest'
data MatchReqHeadersRes
    = MRGood
      -- ^ Headers were indeed requested and precisely match our
      -- request
    | MRUnexpected Text
      -- ^ Headers don't represent valid response to our
      -- request. Reason is attached.
    deriving (Show)

-- TODO This function is used ONLY in recovery mode, so passing the
-- flag is redundant, it's always True.
matchRequestedHeaders
    :: HasConfiguration
    => NewestFirst NE BlockHeader
    -> MsgGetHeaders
    -> Bool
    -> MatchReqHeadersRes
matchRequestedHeaders headers mgh@MsgGetHeaders {..} inRecovery =
    let newTip = headers ^. _NewestFirst . _neHead
        startHeader = headers ^. _NewestFirst . _neLast
        startMatches =
            or [ (startHeader ^. headerHashG) `elem` mghFrom
               , (startHeader ^. prevBlockL) `elem` mghFrom
               ]
        mghToMatches
            | inRecovery = True
            | isNothing mghTo = True
            | otherwise = Just (headerHash newTip) == mghTo
    in if | not startMatches ->
            MRUnexpected $ sformat ("start (from) header "%build%
                                    " doesn't match request "%build)
                                   startHeader mgh
          | not mghToMatches ->
            MRUnexpected $ sformat ("finish (to) header "%build%
                                    " doesn't match request "%build%
                                    ", recovery: "%shown%", newTip:"%build)
                                   mghTo mgh inRecovery newTip
          | otherwise -> MRGood

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

getBlocks
    :: forall d .
       ( DiffusionWorkMode d
       , HasBlockLimits d
       )
    => Logic d
    -> EnqueueMsg d
    -> NodeId
    -> BlockHeader
    -> [HeaderHash]
    -> d (Either GetBlocksError [Block])
getBlocks logic enqueue nodeId tipHeader checkpoints = do
    headers <- requestHeaders
    blocks <- requestBlocks headers
    return (Right (toList blocks))
    -- TODO exception handling? Don't just catchj all.
    -- What exactly is 'GetBlocksError' for?
    -- Maybe we don't need it?
  where
    mgh :: MsgGetHeaders
    mgh = MsgGetHeaders
        { mghFrom = checkpoints
        , mghTo = Just (headerHash tipHeader)
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
                    ("requestHeaders: received "%int%" headers of total size "%builder%
                     " from nodeId "%build%": "%listJson)
                    (headers ^. _NewestFirst . to NE.length)
                    (unitBuilder $ biSize headers)
                    nodeId
                    (map headerHash headers)
                case matchRequestedHeaders headers mgh inRecovery of
                    MRGood           -> return headers
                    MRUnexpected msg -> do
                        logWarning $ sformat
                            ("requestHeaders: headers received were not requested or are invalid"%
                             ", peer id: "%build%", reason:"%stext)
                            nodeId msg
                        logWarning $ sformat
                            ("requestHeaders: unexpected or invalid headers: "%listJson) headers
                        throwM $ DialogUnexpected $
                            sformat ("requestHeaders: received unexpected headers from "%build) nodeId


    requestBlocks :: NewestFirst NE BlockHeader -> d (NewestFirst NE Block)
    requestBlocks headers = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation headers)

    requestBlocksConversation
        :: NewestFirst NE BlockHeader
        -> ConversationActions MsgGetBlocks MsgBlock d
        -> d (NewestFirst NE Block)
    requestBlocksConversation headers conv = do
        -- Preserved behaviour from existing logic code: all of the headers
        -- except for the first and last are tossed away.
        -- TODO don't be so wasteful.
        let oldestHeader = headers ^. _NewestFirst . _neLast
            newestHeader = headers ^. _NewestFirst . _neHead
            lcaChild = oldestHeader
            newestHash = headerHash newestHeader
            lcaChildHash = headerHash lcaChild
        logDebug $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChildHash
                           newestHash
        send conv $ mkBlocksRequest lcaChildHash newestHash
        logDebug "Requested blocks, waiting for the response"
        chainE <- runExceptT (retrieveBlocks conv lcaChild newestHash)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks frmo "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
                                  lcaChildHash newestHash nodeId e
                logWarning msg
                throwM $ DialogUnexpected msg
            Right blocks -> do
                logDebug $ sformat
                    ("Retrieved "%int%" blocks of total size "%builder%": "%listJson)
                    (blocks ^. _NewestFirst . to NE.length)
                    (unitBuilder $ biSize blocks)
                    (map (headerHash . view blockHeader) blocks)
                return blocks

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> BlockHeader
        -> HeaderHash
        -> ExceptT Text d (NewestFirst NE Block)
    retrieveBlocks conv lcaChild endH = do
        blocks <- retrieveBlocksDo 0 conv (lcaChild ^. prevBlockL) endH []
        let b0 = blocks ^. _NewestFirst . _neHead
        if headerHash b0 == headerHash lcaChild
           then pure blocks
           else throwError $ sformat
                    ("First block of chain is "%build%
                     " instead of expected "%build)
                    (b0 ^. blockHeader) lcaChild

    -- Content of retrieveBlocks.
    -- Receive until a block with a given hash 'endH' is found.
    -- Assumes that 'endH' is the hash of a block which is older than 'prevH'.
    -- Retrieval starts with 'prevH'.
    --
    -- Copied from the old logic but modified to use an accumulator rather
    -- than fmapping (<|). That changed the order so we're now NewestFirst
    -- (presumably the server sends them oldest first, as that assumption was
    -- required for the old version to correctly say OldestFirst).
    --
    -- FIXME it strikes me as weird that we check for hash equality to determine
    -- when to stop. Why not stop when the server stops feeding?
    retrieveBlocksDo
        :: Int        -- ^ Index of block we're requesting
        -> ConversationActions MsgGetBlocks MsgBlock d
        -> HeaderHash -- ^ We're expecting a child of this block
        -> HeaderHash -- ^ Block at which to stop
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text d (NewestFirst NE Block)
    retrieveBlocksDo !i conv prevH endH !acc = lift (recvLimited conv) >>= \case
        Nothing ->
            throwError $ sformat ("Failed to receive block #"%int) i
        Just (MsgNoBlock t) ->
            throwError $ sformat ("Server failed to return block #"%int%": "%stext) i t
        Just (MsgBlock block) -> do
            let prevH' = block ^. prevBlockL
                curH = headerHash block
            when (prevH' /= prevH) $ do
                throwError $ sformat
                    ("Received block #"%int%" with prev hash "%shortHashF%
                     " while "%shortHashF%" was expected: "%build)
                    i prevH' prevH (block ^. blockHeader)
            -- FIXME
            -- Something to do with recording progress here.
            -- Perhaps we could restore it by offering a streaming interface
            -- for getBlocks.
            --progressHeaderVar <- view (lensOf @ProgressHeaderTag)
            --atomically $ do void $ tryTakeTMVar progressHeaderVar
            --                putTMVar progressHeaderVar $ block ^. blockHeader
            if curH == endH
            -- Presumably the 'endH' block is the newest, i.e. the server
            -- sends blocks oldest first.
            then pure $ NewestFirst (block :| acc)
            else retrieveBlocksDo (i+1) conv curH endH (block : acc)
            --else over _Wrapped (block <|) <$> retrieveBlocksDo (i+1) conv curH endH

requestTip
    :: forall d t .
       ( DiffusionWorkMode d
       , HasBlockLimits d
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
            Nothing -> throwM $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        k tip nodeId
    handleTip _ t = do
        logWarning $ sformat ("requestTip: got enexpected response: "%shown) t
        throwM $ DialogUnexpected "peer sent more than one tip"

announceBlock
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> EnqueueMsg d
    -> MainBlockHeader
    -> d (Map NodeId (d ()))
announceBlock logic enqueue header =  do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        -- TODO figure out what this security stuff is doing and judge whether
        -- it needs to change / be removed.
        sparams <- securityParams logic
        -- Copied from Pos.Security.Util but made pure. The existing
        -- implementation was tied to a reader rather than taking a
        -- SecurityParams value as a function argument.
        let shouldIgnoreAddress :: NetworkAddress -> Bool
            shouldIgnoreAddress addr = and
                [ AttackNoBlocks `elem` spAttackTypes sparams
                , NetworkAddressTarget addr `elem` spAttackTargets sparams
                ]
            throwOnIgnored :: NodeId -> d ()
            throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    when (shouldIgnoreAddress addr) $
                        throw AttackNoBlocksTriggered
        -- TODO the when condition is not necessary, as it's a part of the
        -- conjunction in shouldIgnoreAddress
        when (AttackNoBlocks `elem` spAttackTypes sparams) (throwOnIgnored nodeId)
        logDebug $
            sformat
                ("Announcing block"%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (Right header))
        handleHeadersCommunication logic cA

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
        -- FIXME recoveryInProgress is a logic layer notion; get it from the
        -- logic layer itself. Better yet: don't use it at all. Diffusion layer
        -- is entirely capable of serving blocks even if the logic layer is in
        -- recovery mode.
        ifM (recoveryInProgress logic) onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Right . one <$> getLastMainHeader
                ([], Just h)  -> do
                    bheader <- getBlockHeader logic h
                    case bheader of
                        Left _ -> pure $ Left "getBlockHeader failed"
                        Right mHeader -> pure . maybeToRight "getBlockHeader returned Nothing" . fmap one $ mHeader
                (c1:cxs, _)   -> do
                    headers <- getBlockHeaders logic (c1:|cxs) mghTo
                    case headers of
                        Left (GetBlockHeadersError txt) -> pure (Left txt)
                        Right hs -> pure (Right hs)
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: d BlockHeader
    getLastMainHeader = do
        etip :: Either GetTipError Block <- getTip logic
        case etip of
            Left err@(GetTipError _) -> throw err
            Right tip -> let tipHeader = tip ^. blockHeader in case tip of
                Left _  -> do
                    bheader <- getBlockHeader logic (tip ^. prevBlockL)
                    case bheader of
                        Left err -> throw err
                        Right mHeader -> pure $ fromMaybe tipHeader mHeader
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

{-
import           Formatting (build, int, sformat, (%))
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic (getHeadersFromToIncl)
import           Pos.Block.Network.Announce (handleHeadersCommunication)
import           Pos.Block.Network.Logic (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders,
                                          MsgHeaders (..))
import           Pos.Communication.Limits (recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec (..),
                                             MkListeners, OutSpecs, constantListeners)
import qualified Pos.DB.Block as DB
import           Pos.DB.Error (DBError (DBMalformed))
-}

-- | All block-related listeners.
blockListeners
    :: ( DiffusionWorkMode m
       , HasBlockLimits m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> MkListeners m
blockListeners logic oq = constantListeners $ map ($ oq)
    [ handleGetHeaders logic
    , handleGetBlocks logic
    , handleBlockHeaders logic
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
    handleHeadersCommunication logic conv --(convToSProxy conv)

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
        mHashes <- getBlockHeaders' logic mgbFrom mgbTo
        case mHashes of
            Right (Just hashes) -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
                for_ hashes $ \hHash ->
                    getBlock logic hHash >>= \case
                        Right (Just b) -> send conv (MsgBlock b)
                        -- TODO handle Left *and* Right Nothing
                        _ -> do
                            send conv (MsgNoBlock $
                                       "Couldn't retrieve block with hash " <> pretty hHash)
                            --failMalformed
                logDebug "handleGetBlocks: blocks sending done"
            _ -> logWarning $ "getBlocksByHeaders@retrieveHeaders returned Nothing"
  {-
  where
    FIXME this makes no sense. Diffusion layer should not make judgements about
    malformedness of the database.
    failMalformed =
        throwM $ DBMalformed $
        "hadleGetBlocks: getHeadersFromToIncl returned header that doesn't " <>
        "have corresponding block in storage."
  -}

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall pack m.
       ( DiffusionWorkMode m
       , HasBlockLimits m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleBlockHeaders logic oq = listenerConv @MsgGetHeaders oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    mHeaders <- recvLimited conv
    whenJust mHeaders $ \case
        (MsgHeaders headers) ->
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
