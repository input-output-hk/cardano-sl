-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       ) where

import           Universum

import           Formatting (build, int, sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary.Communication ()
import           Pos.Block.Logic (getHeadersRange)
import           Pos.Block.Network.Announce (handleHeadersCommunication)
import           Pos.Block.Network.Logic (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders,
                                          MsgHeaders (..))
import           Pos.Communication.Limits (recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec (..),
                                             MkListeners, OutSpecs, constantListeners)
import           Pos.Configuration (recoveryHeadersMessage)
import qualified Pos.DB.Block as DB
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Network.Types (Bucket, NodeId)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.WorkMode.Class (WorkMode)

blockListeners
    :: WorkMode ctx m
    => OQ.OutboundQ pack NodeId Bucket
    -> MkListeners m
blockListeners oq = constantListeners $ map ($ oq)
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]

----------------------------------------------------------------------------
-- Getters (return currently stored data)
----------------------------------------------------------------------------

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall pack ctx m.
       (WorkMode ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication conv --(convToSProxy conv)

handleGetBlocks
    :: forall pack ctx m.
       (WorkMode ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
        -- We fail if we're requested to give more than
        -- recoveryHeadersMessage headers at once.
        getHeadersRange (Just $ recoveryHeadersMessage - 1) mgbFrom mgbTo >>= \case
            Right hashes -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
                for_ hashes $ \hHash ->
                    DB.getBlock hHash >>= \case
                        Nothing -> do
                            send conv (MsgNoBlock $
                                       "Couldn't retrieve block with hash " <> pretty hHash)
                            failMalformed
                        Just b -> send conv (MsgBlock b)
                logDebug "handleGetBlocks: blocks sending done"
            Left e -> do
                let e' = "handleGetBlocks: got Left: " <> e
                logWarning e'
                send conv (MsgNoBlock e')
  where
    failMalformed =
        throwM $ DBMalformed $
        "handleGetBlocks: getHeadersRange returned header that doesn't " <>
        "have corresponding block in storage."

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall pack ctx m.
       WorkMode ctx m
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
