{-# LANGUAGE ScopedTypeVariables #-}
-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKLight
       , sendProxySKHeavy
       , sendProxyConfirmSK
       , sendProxySKLightOuts
       , sendProxySKHeavyOuts
       , sendProxyConfirmSKOuts
       ) where

import           Universum

import qualified Ether
import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logDebug)

import           Pos.Binary.Communication   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Action', Conversation (..),
                                             ConversationActions (..), OutSpecs, convH,
                                             toOutSpecs)
import           Pos.Context                (NodeParams, npSecretKey)
import           Pos.Crypto                 (SignTag (SignProxySK), proxySign)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Discovery.Broadcast    (converseToNeighbors)
import           Pos.Discovery.Class        (MonadDiscovery)
import           Pos.Types                  (ProxySKHeavy, ProxySKLight)
import           Pos.WorkMode.Class         (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKLight
    :: (MinWorkMode m, MonadDiscovery m)
    => ProxySKLight -> Action' m ()
sendProxySKLight = \psk sendActions -> do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions SendProxySK ConfirmProxySK m) ->
            send cA $ SendProxySKLight psk

sendProxySKLightOuts :: OutSpecs
sendProxySKLightOuts = toOutSpecs [convH (Proxy :: Proxy SendProxySK)
                                         (Proxy :: Proxy ConfirmProxySK) ]

-- | Sends simple psk to neighbours
sendProxySKHeavy
    :: (MinWorkMode m, MonadDiscovery m)
    => ProxySKHeavy
    -> Action' m ()
sendProxySKHeavy psk = \sendActions -> do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions SendProxySK ConfirmProxySK m) ->
            send cA $ SendProxySKHeavy psk

sendProxySKHeavyOuts :: OutSpecs
sendProxySKHeavyOuts = toOutSpecs [convH (Proxy :: Proxy SendProxySK)
                                         (Proxy :: Proxy ConfirmProxySK) ]

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK
    :: (WorkMode ssc m)
    => ProxySKLight -> Action' m ()
sendProxyConfirmSK psk = \sendActions -> do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) psk
    sk <- npSecretKey <$> Ether.ask @NodeParams
    let proof = proxySign SignProxySK sk psk psk -- but still proving is
                                                 -- nothing but fear
    converseToNeighbors sendActions $ \_ -> pure . Conversation $
        \(cA :: ConversationActions ConfirmProxySK SendProxySK m) ->
            send cA $ ConfirmProxySK psk proof

sendProxyConfirmSKOuts :: OutSpecs
sendProxyConfirmSKOuts = toOutSpecs [convH (Proxy :: Proxy ConfirmProxySK)
                                           (Proxy :: Proxy SendProxySK) ]
