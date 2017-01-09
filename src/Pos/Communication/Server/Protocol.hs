{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part of protol versioning communication.

module Pos.Communication.Server.Protocol
       ( protocolListeners
       ) where

import           Control.Concurrent.STM.TVar (modifyTVar, readTVar)
import           Control.Lens                (view, (.~), (?~))
import           Formatting                  (build, sformat, (%))
import           Pos.Communication.Types     (MutPeerState, VersionReq (..),
                                              VersionResp (..))
import           System.Wlog                 (logDebug, logWarning)
import           Universum

import           Message.Message             (BinaryP, messageName)
import           Mockable.Monad              (MonadMockable (..))
import           Mockable.SharedAtomic       (modifySharedAtomic, readSharedAtomic)
import           Node                        (Listener (..), ListenerAction (..),
                                              NodeId (..), SendActions (..), sendTo)
import           Pos.Binary.Communication    ()
import           Pos.Communication.BiP       (BiP (..))
import           Pos.Communication.PeerState (getPeerState)
import           Pos.Communication.Types     (peerVersion)
import           Pos.Constants               (curProtocolVersion, protocolMagic)
import           Pos.DHT.Model               (ListenerDHT (..), MonadDHTDialog,
                                              getUserState, replyToNode)
import           Pos.Ssc.Class.Types         (Ssc (..))
import           Pos.WorkMode                (NewWorkMode)

protocolListeners
    :: ( Ssc ssc
       , NewWorkMode ssc m
       )
    => [ListenerAction BiP m]
protocolListeners =
    [ handleVersionReq
    , handleVersionResp
    ]

-- | Handles a response to get current version
handleVersionReq
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ListenerAction BiP m
handleVersionReq = ListenerActionOneMsg $
    \peerId sendActions VersionReq -> do
        logDebug "Got a version request"
        -- Retrieve version and respond with it
        sendTo sendActions peerId $ VersionResp protocolMagic curProtocolVersion
        -- Ask for the other side version also
        peerState <- readSharedAtomic =<< getPeerState peerId
        let haveVersion = isJust $ view peerVersion peerState
        unless haveVersion $ sendTo sendActions peerId $ VersionReq

-- | Handles response on version request, just by overriding socket
-- state `_peerVersion` parameter.
handleVersionResp
    :: forall ssc m.
       (Ssc ssc, NewWorkMode ssc m)
    => ListenerAction BiP m
handleVersionResp = ListenerActionOneMsg $
    \peerId _ resp@VersionResp{..} -> do
        logDebug $ "Handling version response: " <> show resp
        -- peerState <- readSharedAtomic =<< getPeerState peerId
        peerState <- getPeerState peerId
        if vRespMagic /= protocolMagic
        then do
            logWarning $ sformat ("Got VersionResp with magic "%build%
                                 " that's not equal to ours "%build)
                                 vRespMagic protocolMagic
            -- TODO ban node/close connection
            modifySharedAtomic peerState $ \st ->
                return ((peerVersion .~ Nothing) st, ())
        else do
            modifySharedAtomic peerState $ \st ->
                return ((peerVersion ?~ vRespProtocolVersion) st, ())
            logDebug "Successfully handled version response"
