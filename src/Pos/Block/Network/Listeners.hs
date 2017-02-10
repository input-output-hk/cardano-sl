{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Data.Proxy                  (Proxy (..))
import           Data.Reflection             (reify)
import           Data.Tagged                 (Tagged, proxy, unproxy)
import           Formatting                  (build, sformat, (%))
import qualified Node                        as N
import           Serokell.Data.Memory.Units  (Byte)
import           Serokell.Util.Text          (listJson)
import           System.Wlog                 (WithLogger, logDebug, logWarning,
                                              modifyLoggerName)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Block.Logic             (getHeadersFromToIncl)
import           Pos.Block.Network.Announce  (handleHeadersCommunication)
import           Pos.Block.Network.Retrieval (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types     (MsgBlock (..), MsgGetBlocks (..),
                                              MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Protocol  (ConversationActions (..), HandlerSpec (..),
                                              ListenerSpec (..), OutSpecs, PeerData,
                                              listenerConv, mergeLs, messageName)
import           Pos.Communication.Util      (stubListenerConv)
import qualified Pos.DB                      as DB
import           Pos.DB.Error                (DBError (DBMalformed))
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Util                    (NewestFirst (..))
import           Pos.Util.Binary             (LimitedLength (..))
import           Pos.WorkMode                (WorkMode)

blockListeners
    :: (WorkMode ssc m)
    => ([ListenerSpec m], OutSpecs)
blockListeners = mergeLs
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
                                             PeerData
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
    => (ListenerSpec m, OutSpecs)
handleGetHeaders = listenerConv $ \_ peerId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show peerId
    handleHeadersCommunication conv

handleGetBlocks
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleGetBlocks =
    -- NB #put_checkBlockSize: We can use anything as maxBlockSize
    -- here because 'put' doesn't check block size.
    reify (0 :: Byte) $ \(_ :: Proxy s0) ->
    listenerConv $
        \_ __peerId (conv :: ConversationActions
                               (MsgBlock ssc)
                               (LimitedLength s0 MsgGetBlocks) m) -> do
        mBlock <- fmap withLimitedLength <$> recv conv
        whenJust mBlock $ \mgb@MsgGetBlocks{..} -> do
            logDebug $ sformat ("Got request on handleGetBlocks: "%build) mgb
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
            send conv (MsgBlock block)
        logDebug "handleGetBlocks: blocks sending done"

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => (ListenerSpec m, OutSpecs)
handleBlockHeaders = listenerConv $
    \_ peerId conv -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        (mHeaders :: Maybe (MsgHeaders ssc)) <- recv conv
        whenJust mHeaders $ \(MsgHeaders headers) ->
            handleUnsolicitedHeaders (getNewestFirst headers) peerId conv
