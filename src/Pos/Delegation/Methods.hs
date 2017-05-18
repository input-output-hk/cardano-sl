{-# LANGUAGE ScopedTypeVariables #-}
-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKLight
       , sendProxySKHeavy
       , sendProxyConfirmSK
       , sendProxySKOuts
       , sendProxyConfirmSKOuts
       ) where

import           Universum

import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logDebug)

import           Pos.Binary.Communication   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Action', Conversation (..),
                                             ConversationActions (..), OutSpecs, convH,
                                             toOutSpecs)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Discovery.Broadcast    (converseToNeighbors)
import           Pos.Discovery.Class        (MonadDiscovery)
import           Pos.Types                  (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.WorkMode.Class         (MinWorkMode)

-- | Sends epoch psk to neighbours
sendProxySKLight
    :: (MinWorkMode m, MonadDiscovery m)
    => ProxySKLight -> Action' m ()
sendProxySKLight = \psk sendActions -> do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions SendProxySK ConfirmProxySK m) ->
            send cA $ SendProxySKLight psk

sendProxySKOuts :: OutSpecs
sendProxySKOuts = toOutSpecs [convH (Proxy :: Proxy SendProxySK)
                                    (Proxy :: Proxy Void) ]

-- | Sends simple psk to neighbours
sendProxySKHeavy
    :: (MinWorkMode m, MonadDiscovery m)
    => ProxySKHeavy
    -> Action' m ()
sendProxySKHeavy psk = \sendActions -> do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions SendProxySK Void m) ->
            send cA $ SendProxySKHeavy psk

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK
    :: (MinWorkMode m, MonadDiscovery m)
    => ProxySKLight -> ProxySigLight ProxySKLight -> Action' m ()
sendProxyConfirmSK psk confPsk sendActions = do
    logDebug $ sformat ("Sending light psk confirmation to neigbours:\n"%build) psk
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions ConfirmProxySK Void m) ->
            send cA $ ConfirmProxySK psk confPsk

sendProxyConfirmSKOuts :: OutSpecs
sendProxyConfirmSKOuts = toOutSpecs [convH (Proxy :: Proxy ConfirmProxySK)
                                           (Proxy :: Proxy Void) ]
