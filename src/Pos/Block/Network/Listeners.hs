{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Data.Proxy                  (Proxy (..))
import           Formatting                  (sformat, stext, (%))
import           Node                        (ConversationActions (..),
                                              ListenerAction (..), NodeId (..))
import           Serokell.Util.Text          (listJson)
import           System.Wlog                 (WithLogger, logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (ClassifyHeaderRes (..), classifyNewHeader,
                                              getHeadersFromToIncl)
import           Pos.Block.Network.Announce  (handleHeadersCommunication)
import           Pos.Block.Network.Retrieval (addToBlockRequestQueue, mkHeadersRequest,
                                              requestHeaders)
import           Pos.Block.Network.Types     (MsgBlock (..), MsgGetBlocks (..),
                                              MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.BiP       (BiP (..))
import           Pos.Crypto                  (shortHashF)
import qualified Pos.DB                      as DB
import           Pos.DB.Error                (DBError (DBMalformed))
import           Pos.Ssc.Class.Types         (Ssc)
import           Pos.Types                   (BlockHeader, HasHeaderHash (..))
import           Pos.Util                    (stubListenerConv, stubListenerOneMsg)
import           Pos.WorkMode                (WorkMode)

blockListeners
    :: ( WorkMode ssc m )
    => [ListenerAction BiP m]
blockListeners =
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]

blockStubListeners
    :: ( Ssc ssc, WithLogger m )
    => Proxy ssc -> [ListenerAction BiP m]
blockStubListeners p =
    [ stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy (MsgGetHeaders ssc)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgGetBlocks ssc)) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgHeaders ssc)) p
    ]

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP m
handleGetHeaders = ListenerActionConversation $ \__peerId conv ->
    handleHeadersCommunication conv

handleGetBlocks
    :: forall ssc m.
       (Ssc ssc, WorkMode ssc m)
    => ListenerAction BiP m
handleGetBlocks = ListenerActionConversation $ \__peerId conv -> do
    (mGB :: Maybe (MsgGetBlocks ssc)) <- recv conv
    whenJust mGB $ \(MsgGetBlocks {..}) -> do
        logDebug "Got request on handleGetBlocks"
        hashes <- getHeadersFromToIncl mgbFrom mgbTo
        maybe warn (sendBlocks conv) hashes
  where
    warn = logWarning $ "getBlocksByHeaders@retrieveHeaders returned Nothing"
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

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall ssc m.
        (WorkMode ssc m)
    => ListenerAction BiP m
handleBlockHeaders = ListenerActionConversation $
    \peerId conv -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        (mHeaders :: Maybe (MsgHeaders ssc)) <- recv conv
        whenJust mHeaders $ \(MsgHeaders headers) ->
            handleUnsolicitedHeaders headers peerId conv

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => NonEmpty (BlockHeader ssc)
    -> NodeId
    -> ConversationActions (MsgGetHeaders ssc) (MsgHeaders ssc) m
    -> m ()
handleUnsolicitedHeaders (header :| []) peerId conv =
    handleUnsolicitedHeader header peerId conv
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders (h:|hs) _ _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)

handleUnsolicitedHeader
    :: forall ssc m.
       (WorkMode ssc m)
    => BlockHeader ssc
    -> NodeId
    -> ConversationActions (MsgGetHeaders ssc) (MsgHeaders ssc) m
    -> m ()
handleUnsolicitedHeader header peerId conv = do
    logDebug $ sformat
        ("handleUnsolicitedHeader: single header "%shortHashF%" was propagated, processing")
        hHash
    classificationRes <- classifyNewHeader header
    -- TODO: should we set 'To' hash to hash of header or leave it unlimited?
    case classificationRes of
        CHContinues -> do
            logDebug $ sformat continuesFormat hHash
            addToBlockRequestQueue (header :| []) peerId
        CHAlternative -> do
            logInfo $ sformat alternativeFormat hHash
            mghM <- mkHeadersRequest (Just hHash)
            whenJust mghM $ \mgh ->
                requestHeaders mgh peerId conv
        CHUseless reason -> logDebug $ sformat uselessFormat hHash reason
        CHInvalid _ -> do
            logDebug $ sformat ("handleUnsolicited: header "%shortHashF%" is invalid") hHash
            pass -- TODO: ban node for sending invalid block.
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
