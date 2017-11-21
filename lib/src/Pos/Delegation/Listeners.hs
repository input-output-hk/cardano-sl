{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationRelays
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           System.Wlog (logDebug, logWarning)

import           Pos.Binary ()
import           Pos.Communication.Limits ()
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (MsgType (..))
import           Pos.Communication.Relay (DataParams (..), Relay (..))
import           Pos.Core (ProxySKHeavy)
import           Pos.Delegation.Logic (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.WorkMode.Class (WorkMode)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall ctx m. WorkMode ctx m
    => [Relay m]
delegationRelays = [ pskHeavyRelay ]

pskHeavyRelay
    :: WorkMode ctx m
    => Relay m
pskHeavyRelay = Data $ DataParams MsgTransaction $ \_ _ -> handlePsk
  where
    handlePsk :: forall ctx m. WorkMode ctx m => ProxySKHeavy -> m Bool
    handlePsk pSk = do
        logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKHeavy pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        case verdict of
            PHTipMismatch -> do
                -- We're probably updating state over epoch, so
                -- leaders can be calculated incorrectly. This is
                -- really weird and must not happen. We'll just retry.
                logWarning "Tip mismatch happened in delegation db!"
                handlePsk pSk
            PHAdded -> pure True
            PHRemoved -> pure True
            _ -> pure False
