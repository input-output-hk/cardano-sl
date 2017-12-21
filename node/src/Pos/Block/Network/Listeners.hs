{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       ) where

import qualified Data.ByteString.Lazy            as BL (fromChunks)
import           Formatting                      (build, int, sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Conversation               (sendRaw)
import           Serokell.Util.Text              (listJson)
import           System.Wlog                     (logDebug, logWarning)
import           Universum

-- Also brings in Bi instances.
import           Pos.Binary.Communication        (msgBlockPrefix)
import           Pos.Block.Logic                 (getHeadersFromToIncl)
import           Pos.Block.Network.Announce      (handleHeadersCommunication)
import           Pos.Block.Network.Logic         (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types         (MsgBlock (..), MsgGetBlocks (..),
                                                  MsgGetHeaders, MsgHeaders (..))
import           Pos.Communication.Limits        (recvLimited)
import           Pos.Communication.Listener      (listenerConv)
import           Pos.Communication.Protocol      (ConversationActions (..),
                                                  ListenerSpec (..), MkListeners,
                                                  OutSpecs, constantListeners)
import qualified Pos.DB.Block                    as DB
import           Pos.DB.Error                    (DBError (DBMalformed))
import           Pos.Network.Types               (Bucket, NodeId)
import           Pos.Ssc.Class                   (SscWorkersClass)
import           Pos.Util.Chrono                 (NewestFirst (..))
import           Pos.WorkMode.Class              (WorkMode)

blockListeners
    :: forall pack ssc ctx m .
       (SscWorkersClass ssc, WorkMode ssc ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> MkListeners m
blockListeners oq = constantListeners $ map ($ oq)
    [ handleGetHeaders
    , handleGetBlocks (Proxy @ssc)
    , handleBlockHeaders
    ]

----------------------------------------------------------------------------
-- Getters (return currently stored data)
----------------------------------------------------------------------------

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall pack ssc ctx m.
       (WorkMode ssc ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication conv --(convToSProxy conv)

handleGetBlocks
    :: forall pack ssc ctx m .
       ( WorkMode ssc ctx m
       )
    => Proxy ssc
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks (Proxy :: Proxy ssc) oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    -- Must tell GHC what the 'ssc' type is for the conversation.
    let _ = conv :: ConversationActions (MsgBlock ssc) MsgGetBlocks m
    mbMsg <- recvLimited conv
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
        mHashes <- getHeadersFromToIncl @ssc mgbFrom mgbTo
        case mHashes of
            Just hashes -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
                for_ hashes $ \hHash ->
                    DB.blkGetBlockBytes hHash >>= \case
                        Nothing -> do
                            send conv (MsgNoBlock $
                                       "Couldn't retrieve block with hash " <> pretty hHash)
                            failMalformed
                        -- Warning: unsafe magic stopgap measure.
                        -- We don't want to deserialize the block, because that
                        -- is prohibitively expensive. Instead, we read the
                        -- bytes from the database and assume they are
                        -- well-formed, and then throw on the MsgBlock
                        -- encoding prefix so that clients can still decode it.
                        Just bytes ->
                            let prefix = msgBlockPrefix
                            in  sendRaw conv (BL.fromChunks [prefix, bytes])
                logDebug "handleGetBlocks: blocks sending done"
            _ -> logWarning $ "getBlocksByHeaders@retrieveHeaders returned Nothing"
  where
    failMalformed =
        throwM $ DBMalformed $
        "hadleGetBlocks: getHeadersFromToIncl returned header that doesn't " <>
        "have corresponding block in storage."

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall pack ssc ctx m.
       (SscWorkersClass ssc, WorkMode ssc ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleBlockHeaders oq = listenerConv @MsgGetHeaders oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    mHeaders <- recvLimited conv
    whenJust mHeaders $ \case
        (MsgHeaders headers) ->
            handleUnsolicitedHeaders (getNewestFirst headers) nodeId
        _ -> pass -- Why would somebody propagate 'MsgNoHeaders'? We don't care.
