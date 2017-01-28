-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKEpoch
       , sendProxySKSimple
       , sendProxyConfirmSK
       ) where

import           Formatting                 (build, sformat, (%))
import           Pos.Communication.Protocol (SendActions)
import           System.Wlog                (logDebug)
import           Universum

import           Pos.Binary.Communication   ()

import           Pos.Communication.Message  ()

import           Pos.Context                (getNodeContext, ncSecretKey)
import           Pos.Crypto                 (proxySign)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model              (sendToNeighbors)
import           Pos.Types                  (ProxySKEpoch, ProxySKSimple)
import           Pos.WorkMode               (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKEpoch :: (MinWorkMode m) => SendActions m -> ProxySKEpoch -> m ()
sendProxySKEpoch sendActions psk = do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions $ SendProxySKEpoch psk

-- | Sends simple psk to neighbours
sendProxySKSimple :: (MinWorkMode m) => SendActions m -> ProxySKSimple -> m ()
sendProxySKSimple sendActions psk = do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions $ SendProxySKSimple psk

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK :: (WorkMode ss m) => SendActions m -> ProxySKEpoch -> m ()
sendProxyConfirmSK sendActions pSk = do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions $ ConfirmProxySK pSk proof
