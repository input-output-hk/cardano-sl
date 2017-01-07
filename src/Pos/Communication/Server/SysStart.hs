{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Server which handles system start.

module Pos.Communication.Server.SysStart
       ( sysStartReqListener
       , sysStartReqListenerSlave
       , sysStartRespListener
       ) where

import           Control.Concurrent.MVar  (MVar, tryPutMVar)
import           Message.Message          (Serializable)
import           Node                     (Listener, ListenerAction (..),
                                           SendActions (sendTo))

import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.NewDHTModel   ()
import           Pos.Communication.Types  (SysStartRequest (..), SysStartResponse (..))
import           Pos.NewDHT.Model         (BiP, sendToNeighbors)
import           Pos.Types                (Timestamp)
import           Pos.WorkMode             (NewMinWorkMode)

sysStartReqListenerSlave
    :: ( Monad m
       ) => Listener BiP m
sysStartReqListenerSlave = ListenerActionOneMsg $ \_ _ (_ :: SysStartRequest) -> return ()

-- | Listener for 'SysStartRequest' message.
sysStartReqListener
    :: Timestamp -> Listener BiP m
sysStartReqListener sysStart = ListenerActionOneMsg $
    \sender sendActions (_ :: SysStartRequest) ->
         sendTo sendActions sender $ SysStartResponse sysStart

-- | Listener for 'SysStartResponce' message.
sysStartRespListener
    :: ( NewMinWorkMode m
       ) => MVar Timestamp -> Listener BiP m
sysStartRespListener mvar = ListenerActionOneMsg $
    \_ sendActions (SysStartResponse sysStart :: SysStartResponse) ->
        whenM (liftIO . tryPutMVar mvar $ sysStart) $
            sendToNeighbors sendActions $ SysStartResponse sysStart
