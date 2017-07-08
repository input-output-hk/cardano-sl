{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       ) where

import           Formatting                 (build, sformat, (%))
import           Serokell.Util.Text         (listJson)
import           System.Wlog                (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (getHeadersFromToIncl)
import           Pos.Block.Network.Announce (handleHeadersCommunication)
import           Pos.Block.Network.Logic    (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgGetHeaders, MsgHeaders (..))
import           Pos.Communication.Limits   (withLimitedLength')
import           Pos.Communication.Listener (SizedCAHandler (..), convToSProxy,
                                             listenerConv)
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec (..),
                                             MkListeners, OutSpecs, constantListeners)
import qualified Pos.DB.Block               as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Util.Chrono            (NewestFirst (..))
import           Pos.WorkMode.Class         (WorkMode)

blockListeners
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => MkListeners m
blockListeners = constantListeners
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
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleGetHeaders = listenerConv $ \__ourVerInfo ->
  SizedCAHandler $ \nodeId conv -> do
      logDebug $ "handleGetHeaders: request from " <> show nodeId
      handleHeadersCommunication conv (convToSProxy conv)

handleGetBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleGetBlocks = listenerConv $ \__ourVerInfo ->
  SizedCAHandler $ \nodeId conv -> do
    mbMsg <- fmap (withLimitedLength' $ convToSProxy conv) <$> recv conv
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("Got request on handleGetBlocks: "%build%" from "%build)
            mgb nodeId
        mHashes <- getHeadersFromToIncl @ssc mgbFrom mgbTo
        case mHashes of
            Just hashes -> do
                logDebug $ sformat
                    ("handleGetBlocks: started sending blocks to "%build%" one-by-one: "%listJson)
                    nodeId hashes
                for_ hashes $ \hHash -> do
                    block <- maybe failMalformed pure =<< DB.blkGetBlock @ssc hHash
                    send conv (MsgBlock block)
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
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleBlockHeaders = listenerConv @MsgGetHeaders $ \__ourVerInfo ->
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
  SizedCAHandler $ \nodeId conv -> do
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    mHeaders <- fmap (withLimitedLength' $ convToSProxy conv) <$> recv conv
    whenJust mHeaders $ \(MsgHeaders headers) ->
        handleUnsolicitedHeaders (getNewestFirst headers) nodeId
