{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Listeners
       ( blockListeners
       , blockStubListeners
       ) where

import           Data.Proxy                       (Proxy (..))
import           Data.Reflection                  (reify)
import           Formatting                       (sformat, shown, (%))
import           Node                             (ConversationActions (..),
                                                   ListenerAction (..))
import           Serokell.Data.Memory.Units       (Byte)
import           Serokell.Util.Text               (listJson)
import           System.Wlog                      (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication         ()
import           Pos.Block.Logic                  (getHeadersFromToIncl)
import           Pos.Block.Network.Announce       (handleHeadersCommunication)
import           Pos.Block.Network.Retrieval      (handleUnsolicitedHeaders)
import           Pos.Block.Network.Types          (MsgBlock (..), MsgGetBlocks (..),
                                                   MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.BiP            (BiP (..))
import           Pos.Communication.Types.Protocol (VerInfo)
import qualified Pos.DB                           as DB
import           Pos.DB.Error                     (DBError (DBMalformed))
import           Pos.Ssc.Class.Types              (Ssc)
import           Pos.Util                         (NewestFirst (..), stubListenerConv,
                                                   stubListenerOneMsg)
import           Pos.WorkMode                     (WorkMode)

blockListeners
    :: ( WorkMode ssc m )
    => [ListenerAction BiP VerInfo m]
blockListeners =
    [ handleGetHeaders
    , handleGetBlocks
    , handleBlockHeaders
    ]

blockStubListeners
    :: ( Ssc ssc, WithLogger m )
    => Proxy ssc -> [ListenerAction BiP VerInfo m]
blockStubListeners p =
    [ stubListenerConv $ (const Proxy :: Proxy ssc -> Proxy MsgGetHeaders) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy MsgGetBlocks) p
    , stubListenerOneMsg $ (const Proxy :: Proxy ssc -> Proxy (MsgHeaders ssc)) p
    ]

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
    :: forall ssc m.
       (WorkMode ssc m)
    => ListenerAction BiP VerInfo m
handleGetHeaders = ListenerActionConversation $ \_ __peerId conv ->
    handleHeadersCommunication conv

handleGetBlocks
    :: forall ssc m.
       (Ssc ssc, WorkMode ssc m)
    => ListenerAction BiP VerInfo m
handleGetBlocks =
    -- NB #put_checkBlockSize: We can use anything as maxBlockSize
    -- here because 'put' doesn't check block size.
    reify (0 :: Byte) $ \(_ :: Proxy s0) ->
    ListenerActionConversation $
        \_ __peerId (conv :: ConversationActions
                               VerInfo
                               (MsgBlock s0 ssc)
                               MsgGetBlocks m) ->
        whenJustM (recv conv) $ \mgb@MsgGetBlocks{..} -> do
            logDebug $ sformat ("Got request on handleGetBlocks: "%shown) mgb
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
    => ListenerAction BiP VerInfo m
handleBlockHeaders = ListenerActionConversation $
    \_ peerId conv -> do
        logDebug "handleBlockHeaders: got some unsolicited block header(s)"
        (mHeaders :: Maybe (MsgHeaders ssc)) <- recv conv
        whenJust mHeaders $ \(MsgHeaders headers) ->
            handleUnsolicitedHeaders (getNewestFirst headers) peerId conv
