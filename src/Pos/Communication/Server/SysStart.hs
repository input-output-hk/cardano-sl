{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartRespListener
       ) where

import           Pos.DHT                 (ListenerDHT (..), closeResponse, replyToNode)
import           Universum

import           Control.Concurrent.MVar (MVar, tryPutMVar)
import           Control.TimeWarp.Rpc    (BinaryP, MonadDialog)
import           Pos.Communication.Types (SysStartRequest (..), SysStartResponse (..))
import           Pos.Types               (Timestamp)
import           Pos.WorkMode            (MinWorkMode)


sysStartReqListener :: (MonadDialog BinaryP m, MinWorkMode m) => Maybe (Timestamp) -> ListenerDHT m
sysStartReqListener mSysStart = ListenerDHT $
    \(_ :: SysStartRequest) -> do
        replyToNode $ SysStartResponse mSysStart
        closeResponse

sysStartRespListener :: (MonadDialog BinaryP m, MinWorkMode m) => MVar Timestamp -> ListenerDHT m
sysStartRespListener mvar = ListenerDHT $
    \(SysStartResponse mTs :: SysStartResponse) -> do
        maybe (return ()) (liftIO . void . tryPutMVar mvar) mTs
        closeResponse
