{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Data.Reflection            (reify)
import           Data.Tagged                (Tagged, proxy, unproxy)
import           Formatting                 (build, sformat, (%))
import qualified Node                       as N
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Text         (listJson)
import           System.Wlog                (WithLogger, logDebug, logWarning,
                                             modifyLoggerName)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (getHeadersFromToIncl)
import           Pos.Block.Network.Announce (handleHeadersCommunication)
import           Pos.Block.Network.Logic    (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types    (MsgBlock (..), MsgGetBlocks (..),
                                             MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (recvLimited, reifyMsgLimit)
import           Pos.Communication.Protocol (ConversationActions (..), HandlerSpec (..),
                                             ListenerSpec (..), OutSpecs, listenerConv,
                                             mergeLs, messageName)
import           Pos.Communication.Util     (stubListenerConv)
import qualified Pos.DB.Block               as DB
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.Ssc.Class              (SscHelpersClass, SscWorkersClass)
import           Pos.Util                   (NewestFirst (..))
import           Pos.WorkMode               (WorkMode)

blockListeners
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => m ([ListenerSpec m], OutSpecs)
blockListeners = mergeLs <$> sequence
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]

blockStubListeners
    :: ( SscHelpersClass ssc, WithLogger m )
    => Tagged ssc ([ListenerSpec m], OutSpecs)
blockStubListeners = unproxy $ \sscProxy -> mergeLs
    [ stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy (MsgGetHeaders, MsgHeaders ssc)) sscProxy
    , proxy stubListenerConv' sscProxy
    , stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy (MsgHeaders ssc, MsgGetHeaders)) sscProxy
    ]

stubListenerConv'
    :: (SscHelpersClass ssc, WithLogger m)
    => Tagged ssc (ListenerSpec m, OutSpecs)
stubListenerConv' = unproxy $ \(_ :: Proxy ssc) ->
    reify (0 :: Byte) $ \(_ :: Proxy s0) ->
        let rcvName = messageName (Proxy :: Proxy MsgGetBlocks)
            sndName = messageName (Proxy :: Proxy (MsgBlock b))
            listener _ = N.ListenerActionConversation $
              \_d __nId (_convActions :: N.ConversationActions
                                             (MsgBlock ssc)
                                             MsgGetBlocks m) ->
                  modifyLoggerName (<> "stub") $
                        logDebug $ sformat
                            ("Stub listener ("%build%", Conv "%build%"): received message")
                            rcvName
                            sndName
         in (ListenerSpec listener (rcvName, ConvHandler sndName), mempty)

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
        forM_ hashes $ \hHash -> do
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
