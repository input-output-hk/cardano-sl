-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKLight
       , sendProxySKHeavy
       , sendProxyConfirmSK
       , sendProxySKLightOuts
       , sendProxySKHeavyOuts
       , sendProxyConfirmSKOuts
       ) where

import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logDebug)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Action', OutSpecs, oneMsgH, toOutSpecs, NodeId)
import           Pos.Context                (getNodeContext, ncNodeParams, npSecretKey)
import           Pos.Crypto                 (SignTag (SignProxySK), proxySign)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Discovery.Neighbors    (sendToNeighbors)
import           Pos.Types                  (ProxySKHeavy, ProxySKLight)
import           Pos.WorkMode               (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKLight :: (MinWorkMode m)
                 => m (Set NodeId) -> ProxySKLight -> Action' m ()
sendProxySKLight getPeers = \psk sendActions -> do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    peers <- getPeers
    sendToNeighbors peers sendActions $ SendProxySKLight psk

sendProxySKLightOuts :: OutSpecs
sendProxySKLightOuts = toOutSpecs [oneMsgH (Proxy :: Proxy SendProxySK)]

-- | Sends simple psk to neighbours
sendProxySKHeavy
    :: (MinWorkMode m)
    => m (Set NodeId)
    -> ProxySKHeavy
    -> Action' m ()
sendProxySKHeavy getPeers = \psk sendActions -> do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    peers <- getPeers
    sendToNeighbors peers sendActions $ SendProxySKHeavy psk

sendProxySKHeavyOuts :: OutSpecs
sendProxySKHeavyOuts = toOutSpecs [oneMsgH (Proxy :: Proxy SendProxySK)]

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK :: (WorkMode ssc m)
                   => m (Set NodeId) -> ProxySKLight -> Action' m ()
sendProxyConfirmSK getPeers = \psk sendActions -> do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) psk
    sk <- npSecretKey . ncNodeParams <$> getNodeContext
    let proof = proxySign SignProxySK sk psk psk -- but still proving is
                                                 -- nothing but fear
    peers <- getPeers
    sendToNeighbors peers sendActions $ ConfirmProxySK psk proof

sendProxyConfirmSKOuts :: OutSpecs
sendProxyConfirmSKOuts = toOutSpecs [oneMsgH (Proxy :: Proxy ConfirmProxySK)]
