{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods.

module Pos.Communication.Methods
       (
       -- * Sending data into network
         sendProxySecretKey
       , sendProxyConfirmSK
       ) where

import           Formatting               (build, sformat, (%))
import           Mockable                 (fork)
import           Node                     (SendActions (..))
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Class         (Bi)
import           Pos.Binary.Communication ()
import           Pos.Binary.Types         ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Delegation.Types     (ConfirmProxySK (..), SendProxySK (..))
import           Pos.NewDHT.Model         (sendToNeighbors)
import           Pos.Types                (ProxySKEpoch)
import           Pos.WorkMode             (NewMinWorkMode)

-- [CSL-514] TODO Log long acting sends
-- sendToNeighborsSafe :: (NewMinWorkMode ssc m, Message r) => SendActions BiP m -> r -> m ()
-- sendToNeighborsSafe sendActions msg = do
--     void $ fork $
--         logWarningWaitLinear 10
--             ("Sending " <> messageName' msg <> " to neighbors") $
--             sendToNeighbors sendActions msg
