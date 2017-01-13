{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Control.Concurrent.STM.TBQueue (isFullTBQueue, writeTBQueue)
import           Control.Lens                   ((^.))
import           Data.List.NonEmpty             (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty             as NE
import           Data.Proxy                     (Proxy (..))
import           Formatting                     (sformat, shown, stext, (%))
import           Node                           (ConversationActions (..),
                                                 ListenerAction (..), NodeId (..),
                                                 SendActions (..))
import           Serokell.Util.Text             (listJson)
import           Serokell.Util.Verify           (isVerSuccess)
import           System.Wlog                    (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication       ()
import           Pos.Block.Logic                (ClassifyHeaderRes (..),
                                                 ClassifyHeadersRes (..), classifyHeaders,
                                                 classifyNewHeader, getHeadersFromManyTo,
                                                 getHeadersFromToIncl, getHeadersOlderExp)
import           Pos.Block.Network.Types        (InConv (..), MsgBlock (..),
                                                 MsgGetBlocks (..), MsgGetHeaders (..),
                                                 MsgHeaders (..))
import           Pos.Communication.BiP          (BiP (..))
import           Pos.Context                    (getNodeContext, ncBlockRetreivalQueue)
import           Pos.Crypto                     (hash, shortHashF)
import qualified Pos.DB                         as DB
import           Pos.DB.Error                   (DBError (DBMalformed))
import           Pos.DHT.Model                  (nodeIdToAddress)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Types                      (BlockHeader, HasHeaderHash (..),
                                                 HeaderHash, verifyHeaders)
import           Pos.Util                       (_neHead, _neLast)
import           Pos.Util                       (stubListenerConv, stubListenerOneMsg)
import           Pos.WorkMode                   (WorkMode)

blockListeners
    :: ( WorkMode ssc m )
    => [ListenerAction BiP m]
blockListeners =
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]
blockStubListeners
    :: ( Ssc ssc, Monad m )
    => Proxy ssc -> [ListenerAction BiP m]
blockStubListeners p =
    [ stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy (MsgGetHeaders ssc)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgGetBlocks ssc)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgHeaders ssc)) p
    ]

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe (HeaderHash ssc) -> m (MsgGetHeaders ssc)
mkHeadersRequest upto = do
    headers <- getHeadersOlderExp Nothing
    pure $ MsgGetHeaders headers upto

matchRequestedHeaders
    :: (Ssc ssc)
    => NonEmpty (BlockHeader ssc) -> MsgGetHeaders ssc -> Bool
matchRequestedHeaders headers@(newTip :| hs) mgh =
    let startHeader = NE.last headers
        startMatches = hash startHeader `elem` mghFrom mgh
        mghToMatches = Just (hash newTip) == mghTo mgh
     in and [ startMatches
            , mghToMatches
            , formChain
            ]
  where
    formChain = isVerSuccess (verifyHeaders True $ newTip:hs)

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleGetHeaders = ListenerActionConversation $
    \__peerId __sendActions conv -> do
        (msg :: Maybe (MsgGetHeaders ssc)) <- recv conv
        whenJust msg $ \(MsgGetHeaders {..}) -> do
            logDebug "Got request on handleGetHeaders"
            headers <- getHeadersFromManyTo mghFrom mghTo
            case nonEmpty headers of
                Nothing ->
                    logWarning $ "handleGetHeaders@retrieveHeadersFromTo returned empty " <>
                                 "list, not responding to node"
                Just atLeastOneHeader ->
                    send conv $ InConv $ MsgHeaders atLeastOneHeader

handleGetBlocks
    :: forall ssc m.
       (Ssc ssc, WorkMode ssc m)
    => ListenerAction BiP m
handleGetBlocks = ListenerActionConversation $
    \__peerId __sendActions conv -> do
        (mGB :: Maybe (MsgGetBlocks ssc)) <- recv conv
        whenJust mGB $ \(MsgGetBlocks {..}) -> do
            logDebug "Got request on handleGetBlocks"
            hashes <- getHeadersFromToIncl mgbFrom mgbTo
            maybe warn (sendBlocks conv) hashes
      where
        warn = logWarning $ "getBLocksByHeaders@retrieveHeaders returned Nothing"
        failMalformed =
            throwM $ DBMalformed $
            "hadleGetBlocks: getHeadersFromToIncl returned header that doesn't " <>
            "have corresponding block in storage."
        sendBlocks conv hashes = do
            logDebug $ sformat
                ("handleGetBlocks: started sending blocks one-by-one: "%listJson) hashes
            forM_ hashes $ \hHash -> do
                block <- maybe failMalformed pure =<< DB.getBlock hHash
                send conv $ MsgBlock block
            logDebug "handleGetBlocks: blocks sending done"

addToBlockRequestQueue
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> m ()
addToBlockRequestQueue headers peerId = do
    queue <- ncBlockRetreivalQueue <$> getNodeContext
    added <- liftIO . atomically $
                ifM (isFullTBQueue queue)
                    (pure False)
                    (True <$ writeTBQueue queue (peerId, headers))
    if added
      then logDebug $ sformat
        ("Added to block request queue: peerId="%shown%", headers="%listJson) peerId headers
      else logWarning $ sformat
        ("Failed to add headers from "%shown%" to block retrieval queue: queue is full")
        peerId

-- First case of 'handleBlockheaders'
handleRequestedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> m ()
handleRequestedHeaders headers peerId = do
    logDebug "handleRequestedHeaders: headers were requested, will process"
    classificationRes <- classifyHeaders headers
    let newestHeader = headers ^. _neHead
        newestHash = headerHash newestHeader
        oldestHash = headerHash $ headers ^. _neLast
    case classificationRes of
        CHsValid lcaChild -> do
            let lcaChildHash = hash lcaChild
            logDebug $ sformat validFormat lcaChildHash newestHash
            addToBlockRequestQueue headers peerId
        CHsUseless reason ->
            logDebug $ sformat uselessFormat oldestHash newestHash reason
        CHsInvalid _ -> pass -- TODO: ban node for sending invalid block.
  where
    validFormat =
        "Received valid headers, can request blocks from " %shortHashF % " to " %shortHashF
    uselessFormat =
        "Chain of headers from " %shortHashF % " to " %shortHashF %
        " is useless for the following reason: " %stext

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleUnsolicitedHeaders (header :| []) peerId sendActions =
    handleUnsolicitedHeader header peerId sendActions
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> SendActions BiP m
    -> m ()
handleUnsolicitedHeader header peerId sendActions = do
    logDebug "handleUnsolicitedHeader: single header was propagated, processing"
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            addToBlockRequestQueue (header :| []) peerId
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            mgh <- mkHeadersRequest (Just hHash)
            withConnectionTo sendActions peerId $ \conv -> do
                logDebug "handleUnsolicitedHeader: withConnection: sending MsgGetHeaders"
                send conv mgh
                logDebug "handleUnsolicitedHeader: withConnection: receiving MsgHeaders"
                mHeaders <- fmap inConvMsg <$> recv conv
                logDebug "handleUnsolicitedHeader: withConnection: received MsgHeaders"
                whenJust mHeaders $ \headers -> do
                    logDebug "handleUnsolicitedHeader: got some block headers"
                    if matchRequestedHeaders headers mgh
                       then handleRequestedHeaders headers peerId
                       else handleUnexpected headers peerId
        CHUseless reason -> logDebug $ sformat uselessFormat hHash reason
        CHInvalid _ -> pass -- TODO: ban node for sending invalid block.
  where
    hHash = headerHash header
    continuesFormat =
        "Header " %shortHashF %
        " is a good continuation of our chain, requesting it"
    alternativeFormat =
        "Header " %shortHashF %
        " potentially represents good alternative chain, requesting more headers"
    uselessFormat =
        "Header " %shortHashF % " is useless for the following reason: " %stext
    handleUnexpected (h:|hs) _ = do
        -- TODO: ban node for sending unsolicited header in conversation
        logWarning $ sformat
            ("handleUnsolicitedHeader: headers received were not requested, address: " % shown)
            (nodeIdToAddress peerId)
        logWarning $ sformat ("handleUnsolicitedHeader: unexpected headers: "%listJson) (h:hs)

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall ssc m.
        (WorkMode ssc m)
    => ListenerAction BiP m
handleBlockHeaders = ListenerActionOneMsg $
    \peerId sendActions (MsgHeaders headers :: MsgHeaders ssc) -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        handleUnsolicitedHeaders headers peerId sendActions
