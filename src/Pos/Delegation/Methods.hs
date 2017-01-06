-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKEpoch
       , sendProxySKSimple
       , sendProxyConfirmSK
       ) where

import           Formatting                (build, sformat, (%))
import           System.Wlog               (logDebug)
import           Universum

import           Pos.Communication.Methods (sendToNeighborsSafe)
import           Pos.Context               (getNodeContext, ncSecretKey)
import           Pos.Crypto                (proxySign)
import           Pos.Delegation.Types      (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Types                 (ProxySKEpoch, ProxySKSimple)
import           Pos.WorkMode              (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKEpoch :: (MinWorkMode ss m) => ProxySKEpoch -> m ()
sendProxySKEpoch psk = do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    sendToNeighborsSafe $ SendProxySKEpoch psk

-- | Sends simple psk to neighbours
sendProxySKSimple :: (MinWorkMode ss m) => ProxySKSimple -> m ()
sendProxySKSimple psk = do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    sendToNeighborsSafe $ SendProxySKSimple psk

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK :: (WorkMode ss m) => ProxySKEpoch -> m ()
sendProxyConfirmSK pSk = do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    sendToNeighborsSafe $ ConfirmProxySK pSk proof
