{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       (
       -- * Sending data into network
         sendToNeighborsSafe
       , sendToNeighborsSafeWithMaliciousEmulation
       , sendTx
       , sendProxySecretKey
       , sendProxyConfirmSK
       ) where

import           Control.TimeWarp.Rpc        (Message, NetworkAddress)
import           Control.TimeWarp.Timed      (fork_)
import           Formatting                  (build, sformat, (%))
import           System.Wlog                 (logDebug)
import           Universum

import           Pos.Binary.Class            (Bi)
import           Pos.Binary.Communication    ()
import           Pos.Binary.Txp              ()
import           Pos.Binary.Types            ()
import           Pos.Communication.Types     (ConfirmProxySK (..), SendProxySK (..))
import           Pos.Context                 (getNodeContext, ncAttackTypes)
import           Pos.Crypto                  (ProxySecretKey)
import           Pos.DHT.Model               (MonadMessageDHT, defaultSendToNeighbors,
                                              sendToNeighbors, sendToNode)
import           Pos.Security                (AttackType (..), shouldIgnoreAddress)
import           Pos.Txp.Types.Communication (TxDataMsg (..))
import           Pos.Types                   (EpochIndex, HeaderHash, MainBlockHeader,
                                              TxAux, headerHash)
import           Pos.Util                    (logWarningWaitLinear, messageName')
import           Pos.WorkMode                (MinWorkMode, WorkMode)

-- thread and controls how much time action takes.
sendToNeighborsSafeImpl :: (Message r, MinWorkMode ssc m) => (r -> m ()) -> r -> m ()
sendToNeighborsSafeImpl sender msg = do
    let msgName = messageName' msg
    let action = () <$ sender msg
    fork_ $
        logWarningWaitLinear 10 ("Sending " <> msgName <> " to neighbors") action

sendToNeighborsSafe :: (Bi r, Message r, MinWorkMode ssc m) => r -> m ()
sendToNeighborsSafe = sendToNeighborsSafeImpl $ void . sendToNeighbors

sendToNeighborsSafeWithMaliciousEmulation :: (Bi r, Message r, WorkMode ssc m) => r -> m ()
sendToNeighborsSafeWithMaliciousEmulation msg = do
    cont <- getNodeContext
    -- [CSL-336] Make this parallel
    let sender = if AttackNoBlocks `elem` ncAttackTypes cont
                 then defaultSendToNeighbors sequence (sendToNode' cont)
                 else sendToNeighbors
    sendToNeighborsSafeImpl (void . sender) msg
  where
    sendToNode' cont addr message =
        unless (shouldIgnoreAddress cont addr) $
            sendToNode addr message

-- | Send Tx to given address.
sendTx :: (MonadMessageDHT s m) => NetworkAddress -> TxAux -> m ()
sendTx addr (tx,w,d) = sendToNode addr $ TxDataMsg tx w d

-- | Sends proxy secret key to neighbours
sendProxySecretKey
    :: (MinWorkMode ss m)
    => ProxySecretKey (EpochIndex, EpochIndex) -> m ()
sendProxySecretKey psk = do
    logDebug $ sformat ("Sending proxySecretKey to neigbours:\n"%build) psk
    sendToNeighborsSafe $ SendProxySK psk

sendProxyConfirmSK :: (MinWorkMode ss m) => ConfirmProxySK -> m ()
sendProxyConfirmSK confirmPSK@(ConfirmProxySK psk _) = do
    logDebug $
        sformat ("Sending proxy receival confirmation for psk "%build%" to neigbours") psk
    sendToNeighborsSafe confirmPSK
