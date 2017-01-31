-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKEpoch
       , sendProxySKSimple
       , sendProxyConfirmSK
       , sendProxySKEpochOuts
       , sendProxySKSimpleOuts
       , sendProxyConfirmSKOuts
       ) where

import           Data.Proxy                 (Proxy (..))
import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logDebug)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Action', OutSpecs, oneMsgH, toOutSpecs)
import           Pos.Context                (getNodeContext, ncSecretKey)
import           Pos.Crypto                 (proxySign)
import           Pos.Delegation.Types       (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model              (sendToNeighbors)
import           Pos.Types                  (ProxySKEpoch, ProxySKSimple)
import           Pos.WorkMode               (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKEpoch :: (MinWorkMode m)
                 => ProxySKEpoch -> Action' m ()
sendProxySKEpoch = \psk sendActions -> do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    sendToNeighbors sendActions $ SendProxySKEpoch psk

sendProxySKEpochOuts :: OutSpecs
sendProxySKEpochOuts = toOutSpecs [oneMsgH (Proxy :: Proxy SendProxySK)]

-- | Sends simple psk to neighbours
sendProxySKSimple :: (MinWorkMode m)
                  => ProxySKSimple -> Action' m ()
sendProxySKSimple = \psk sendActions -> do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    sendToNeighbors sendActions $ SendProxySKSimple psk

sendProxySKSimpleOuts :: OutSpecs
sendProxySKSimpleOuts = toOutSpecs [oneMsgH (Proxy :: Proxy SendProxySK)]

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK :: (WorkMode ssc m)
                   => ProxySKEpoch -> Action' m ()
sendProxyConfirmSK = \psk sendActions -> do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) psk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk psk psk -- but still proving is nothing but fear
    sendToNeighbors sendActions $ ConfirmProxySK psk proof

sendProxyConfirmSKOuts :: OutSpecs
sendProxyConfirmSKOuts = toOutSpecs [oneMsgH (Proxy :: Proxy ConfirmProxySK)]
