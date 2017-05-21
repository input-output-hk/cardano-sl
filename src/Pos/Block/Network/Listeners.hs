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
                                             MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec (..),
                                             OutSpecs, listenerConv, mergeLs)
import qualified Pos.DB.Block               as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.Ssc.Class              (SscWorkersClass)
import           Pos.Util.Chrono            (NewestFirst (..))
import           Pos.WorkMode.Class         (WorkMode)

blockListeners
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => m ([ListenerSpec m], OutSpecs)
blockListeners = mergeLs <$> sequence
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => m (ListenerSpec m, OutSpecs)
handleGetHeaders = reifyMsgLimit (Proxy @MsgGetHeaders) $ \limitProxy ->
    return $ listenerConv $ \_ peerId conv -> do
        logDebug $ "handleGetHeaders: request from " <> show peerId
        handleHeadersCommunication conv limitProxy

handleGetBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => m (ListenerSpec m, OutSpecs)
handleGetBlocks = return $ listenerConv $
    \_ __peerId (conv::ConversationActions (MsgBlock ssc) (MsgGetBlocks) m) ->
    whenJustM (recv conv) $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("Got request on handleGetBlocks: "%build) mgb
        hashes <- getHeadersFromToIncl @ssc mgbFrom mgbTo
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
        for_ hashes $ \hHash -> do
            block <- maybe failMalformed pure =<< DB.getBlock hHash
            send conv (MsgBlock block)
        logDebug "handleGetBlocks: blocks sending done"

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall ssc m.
       (SscWorkersClass ssc, WorkMode ssc m)
    => m (ListenerSpec m, OutSpecs)
handleBlockHeaders = reifyMsgLimit (Proxy @(MsgHeaders ssc)) $
    \(_ :: Proxy s) -> return $ listenerConv $
      \_ peerId conv -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        mHeaders <- recvLimited @s conv
        whenJust mHeaders $ \(MsgHeaders headers) ->
            handleUnsolicitedHeaders (getNewestFirst headers) peerId conv
