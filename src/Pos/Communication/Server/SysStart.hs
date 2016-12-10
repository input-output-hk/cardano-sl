{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartReqListenerSlave
       , sysStartRespListener
       ) where

import           Control.Concurrent.MVar (MVar, tryPutMVar)
import           Universum

import           Pos.Binary.Class        (Bi)
import           Pos.Communication.Types (SysStartRequest (..), SysStartResponse (..))
import           Pos.DHT                 (ListenerDHT (..), closeResponse, replyToNode)
import           Pos.Types               (Timestamp)
import           Pos.WorkMode            (MinWorkMode, MonadUserDialog)

sysStartReqListenerSlave :: (MonadUserDialog m, Bi SysStartRequest) => ListenerDHT m
sysStartReqListenerSlave = ListenerDHT $ \(_ :: SysStartRequest) -> return ()

-- | Listener for 'SysStartRequest' message.
sysStartReqListener
    :: (MonadUserDialog m, MinWorkMode m, Bi SysStartRequest, Bi SysStartResponse)
    => Timestamp -> ListenerDHT m
sysStartReqListener sysStart = ListenerDHT $
    \(_ :: SysStartRequest) -> do
        replyToNode $ SysStartResponse sysStart Nothing
        closeResponse

-- | Listener for 'SysStartResponce' message.
sysStartRespListener
    :: (MonadUserDialog m
       ,MinWorkMode m
       ,Bi SysStartResponse)
    => MVar Timestamp -> ListenerDHT m
sysStartRespListener mvar = ListenerDHT $
    \(SysStartResponse ts _ :: SysStartResponse) -> do
        liftIO . void . tryPutMVar mvar $ ts
        closeResponse
