{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part of protol versioning communication.

module Pos.Communication.Server.Protocol
       ( protocolListeners
       ) where

import           Control.Concurrent.STM.TVar (modifyTVar, readTVar)
import           Control.Lens                (view, (.~), (?~))
import           Formatting                  (build, sformat, (%))
import           Pos.Communication.Types     (MutSocketState, VersionReq (..),
                                              VersionResp (..))
import           System.Wlog                 (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Communication.Types     (ResponseMode, peerVersion)
import           Pos.Constants               (curProtocolVersion, protocolMagic)
import           Pos.DHT.Model               (ListenerDHT (..), MonadDHTDialog,
                                              getUserState, replyToNode)
import           Pos.WorkMode                (WorkMode)
import           Node                        (Listener(..), ListenerAction(..), sendTo,
                                              NodeId(..), SendActions(..))
import           Message.Message             (BinaryP, messageName)
import           Mockable.Monad              (MonadMockable(..))
import           Pos.Communication.BiP       (BiP(..))
import           Pos.Ssc.Class.Types         (Ssc(..))

protocolListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
protocolListeners = notImplemented

{-
protocolListeners'
    :: ( Typeable ssc
       , Ssc ssc
       , MonadDHTDialog (MutSocketState ssc) m
       , ResponseMode ssc m
       , MonadMockable m
       , WorkMode ssc m
       )
    => [ListenerAction BiP m]
protocolListeners' =
    [ handleVersionReq
    , handleVersionResp
    ]

-- | Handles a response to get current version
handleVersionReq
    :: forall ssc m.
       ( Typeable ssc
       , Ssc ssc
       , MonadMockable m
       , ResponseMode ssc m
       , WorkMode ssc m
       )
    => ListenerAction BiP m
handleVersionReq = ListenerActionOneMsg $
    \peerId sendActions VersionReq -> do
        logDebug "Got a version request"
        -- Retrieve version and respond with it
        sendTo sendActions peerId $ VersionResp protocolMagic curProtocolVersion
        -- Ask for the other side version also
        stateVar <- getUserState
        haveVersion <-
            isJust . view peerVersion <$> atomically (readTVar stateVar)
        unless haveVersion $ sendTo sendActions peerId $ VersionReq

-- | Handles response on version request, just by overriding socket
-- state `_peerVersion` parameter.
handleVersionResp
    :: forall ssc m.
       ( Typeable ssc
       , Ssc ssc
       , MonadMockable m
       , ResponseMode ssc m
       , WorkMode ssc m
       )
    => ListenerAction BiP m
handleVersionResp = ListenerActionOneMsg $
    \_ _ resp@VersionResp{..} -> do
        logDebug $ "Handling version response: " <> show resp
        stateVar <- getUserState
        if vRespMagic /= protocolMagic
        then do
            logWarning $ sformat ("Got VersionResp with magic "%build%
                                  " that's not equal to ours "%build)
                                 vRespMagic protocolMagic
             -- TODO ban node/close connection
            atomically $ modifyTVar stateVar $ peerVersion .~ Nothing
        else do
            atomically $ modifyTVar stateVar $ peerVersion ?~ vRespProtocolVersion
            logDebug "Successfully handled version response"
-}
