{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartRespListener
       , handleSysStartResp
       ) where

import           Universum

import           Formatting                 (build, sformat, shown, (%))
import           System.Wlog                (logInfo)

import           Pos.Binary.Communication   ()
import           Pos.Binary.Infra.DHTModel  ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (ConversationActions (..), ListenerSpec,
                                             NodeId, OutSpecs, SendActions, listenerConv,
                                             listenerOneMsg, oneMsgH, toOutSpecs)
import           Pos.Communication.Types    (SysStartRequest (..), SysStartResponse (..))
import           Pos.DHT.Model              (sendToNeighbors)
import           Pos.Types                  (Timestamp)
import           Pos.WorkMode               (MinWorkMode)

-- | Listener for 'SysStartRequest' message.
sysStartReqListener
    :: MinWorkMode m
    => Timestamp -> (ListenerSpec m, OutSpecs)
sysStartReqListener sysStart = listenerConv $
    \_ peerId conv  -> do
        (mReq :: Maybe SysStartRequest) <- recv conv
        whenJust mReq $ \_ -> do
            logInfo $ sformat
                ("Received sysStart request from "%shown%", sending "%build)
                peerId sysStart
            send conv $ SysStartResponse sysStart

-- | Listener for 'SysStartResponce' message.
sysStartRespListener :: MinWorkMode m => MVar Timestamp -> (ListenerSpec m, OutSpecs)
sysStartRespListener mvar = listenerOneMsg outSpecs . const $ handler mvar
  where
    (handler, outSpecs) = handleSysStartResp

handleSysStartResp
    :: MinWorkMode m
    => (MVar Timestamp -> NodeId -> SendActions m -> SysStartResponse -> m (), OutSpecs)
handleSysStartResp = (,outSpecs) $
    \mvar peerId sendActions (SysStartResponse sysStart) -> do
        logInfo $ sformat
            ("Received sysStart response from "%shown%", "%build)
            peerId sysStart
        whenM (liftIO . tryPutMVar mvar $ sysStart) $
            sendToNeighbors sendActions $ SysStartResponse sysStart
  where
    outSpecs = toOutSpecs [oneMsgH (Proxy :: Proxy SysStartResponse)]
