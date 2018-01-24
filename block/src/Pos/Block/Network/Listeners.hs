{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BL (fromChunks)
import           Formatting (build, int, sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Conversation (sendRaw)
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary.Block.Network (msgBlockPrefix)
import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Configuration (recoveryHeadersMessage)
import           Pos.Block.Logic (getHeadersFromManyTo, getHeadersRange)
import           Pos.Block.Network.Logic (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                          MsgHeaders (..))
import           Pos.Communication.Limits.Types (recvLimited)
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec (..),
                                             MkListeners, OutSpecs, constantListeners)
import           Pos.Core (Block, BlockHeader, blockHeader, prevBlockL)
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.DB.Class as DB
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Network.Types (Bucket, NodeId)
import           Pos.Recovery.Info (recoveryInProgress)
import           Pos.Util.Chrono (NewestFirst (..))

blockListeners
    :: forall pack ctx m .
       (BlockWorkMode ctx m)
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
       (BlockWorkMode ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("handleGetHeaders: got request "%build%" from "%build)
            mgh nodeId
        ifM recoveryInProgress (onRecovery conv) $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Right . one <$> getLastMainHeader
                ([], Just h)  ->
                    maybeToRight "getBlockHeader returned Nothing" . fmap one <$>
                    DB.getHeader h
                (c1:cxs, _)   ->
                    first ("getHeadersFromManyTo: " <>) <$>
                    runExceptT (getHeadersFromManyTo (c1:|cxs) mghTo)
            either (onNoHeaders conv) (handleSuccess conv) headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: BlockWorkMode ctx m => m BlockHeader
    getLastMainHeader = do
        tip :: Block <- DB.getTipBlock
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> fromMaybe tipHeader <$> DB.getHeader (tip ^. prevBlockL)
            Right _ -> pure tipHeader
    handleSuccess conv h = do
        send conv (MsgHeaders h)
        logDebug "handleGetHeaders: responded successfully"
    onNoHeaders conv reason = do
        let err = "handleGetHeaders: couldn't retrieve headers, reason: " <> reason
        logWarning err
        send conv (MsgNoHeaders err)
    onRecovery conv = do
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
        send conv (MsgNoHeaders "server node is in recovery mode")


handleGetBlocks
    :: forall pack ctx m.
       (BlockWorkMode ctx m)
    => OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    whenJustM (recvLimited conv) $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
        -- We fail if we're requested to give more than
        -- recoveryHeadersMessage headers at once.
        getHeadersRange (Just recoveryHeadersMessage) mgbFrom mgbTo >>= \case
            Right hashes -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
                for_ hashes $ \hHash ->
                    DB.dbGetSerBlock hHash >>= \case
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
                        Just (DB.Serialized bytes) ->
                            let prefix = msgBlockPrefix
                            in  sendRaw conv (BL.fromChunks [prefix, bytes])
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
       BlockWorkMode ctx m
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
