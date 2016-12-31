{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartReqListenerSlave
       , sysStartRespListener
       ) where

import           Control.Concurrent.MVar  (MVar, tryPutMVar)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Communication.Types  (SysStartRequest (..), SysStartResponse (..))
import           Pos.DHT.Model            (ListenerDHT (..), MonadDHTDialog,
                                           closeResponse, replyToNode)
import           Pos.Types                (Timestamp)
import           Pos.WorkMode             (MinWorkMode)

sysStartReqListenerSlave :: (MonadDHTDialog s m) => ListenerDHT s m
sysStartReqListenerSlave = ListenerDHT $ \(_ :: SysStartRequest) -> return ()

-- | Listener for 'SysStartRequest' message.
sysStartReqListener
    :: (MonadDHTDialog ss m, MinWorkMode ss m)
    => Timestamp -> ListenerDHT ss m
sysStartReqListener sysStart = ListenerDHT $
    \(_ :: SysStartRequest) -> do
        replyToNode $ SysStartResponse sysStart Nothing
        closeResponse

-- | Listener for 'SysStartResponce' message.
sysStartRespListener
    :: (MonadDHTDialog ss m, MinWorkMode ss m)
    => MVar Timestamp -> ListenerDHT ss m
sysStartRespListener mvar = ListenerDHT $
    \(SysStartResponse ts _ :: SysStartResponse) -> do
        liftIO . void . tryPutMVar mvar $ ts
        closeResponse
