{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartRespListener
       ) where

import           Control.Concurrent.MVar  (MVar, tryPutMVar)
import           Node                     (Listener, ListenerAction (..),
                                           SendActions (sendTo))

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.DHTModel   ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Communication.Types  (SysStartRequest (..), SysStartResponse (..))
import           Pos.DHT.Model         (sendToNeighbors)
import           Pos.Types                (Timestamp)
import           Pos.WorkMode             (MinWorkMode)

-- | Listener for 'SysStartRequest' message.
sysStartReqListener
    :: Timestamp -> Listener BiP m
sysStartReqListener sysStart = ListenerActionOneMsg $
    \sender sendActions (_ :: SysStartRequest) ->
         sendTo sendActions sender $ SysStartResponse sysStart

-- | Listener for 'SysStartResponce' message.
sysStartRespListener
    :: ( MinWorkMode m
       ) => MVar Timestamp -> Listener BiP m
sysStartRespListener mvar = ListenerActionOneMsg $
    \_ sendActions (SysStartResponse sysStart :: SysStartResponse) ->
        whenM (liftIO . tryPutMVar mvar $ sysStart) $
            sendToNeighbors sendActions $ SysStartResponse sysStart
