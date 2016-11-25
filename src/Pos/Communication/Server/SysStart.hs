{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartReqListenerSlave
       , sysStartRespListener
       ) where

import           Pos.DHT                 (ListenerDHT (..), closeResponse, replyToNode)
import           Universum

import           Control.Concurrent.MVar (MVar, tryPutMVar)
import           Control.TimeWarp.Rpc    (BinaryP, MonadDialog)
import           Pos.Communication.Types (SysStartRequest (..), SysStartResponse (..))
import           Pos.Types               (Timestamp)
import           Pos.WorkMode            (MinWorkMode)

sysStartReqListenerSlave :: (MonadDialog BinaryP m) => ListenerDHT m
sysStartReqListenerSlave = ListenerDHT $ \(_ :: SysStartRequest) -> return ()

-- | Listener for 'SysStartRequest' message.
sysStartReqListener :: (MonadDialog BinaryP m, MinWorkMode m) => Timestamp -> ListenerDHT m
sysStartReqListener sysStart = ListenerDHT $
    \(_ :: SysStartRequest) -> do
        replyToNode $ SysStartResponse sysStart Nothing
        closeResponse

-- | Listener for 'SysStartResponce' message.
sysStartRespListener :: (MonadDialog BinaryP m, MinWorkMode m) => MVar Timestamp -> ListenerDHT m
sysStartRespListener mvar = ListenerDHT $
    \(SysStartResponse ts _ :: SysStartResponse) -> do
        liftIO . void . tryPutMVar mvar $ ts
        closeResponse
