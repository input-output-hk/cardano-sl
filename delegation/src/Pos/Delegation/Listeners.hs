{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic.

module Pos.Delegation.Listeners
       ( handlePsk
       , DlgListenerConstraint
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           Mockable (CurrentTime, Delay, Mockable)
import           System.Wlog (WithLogger, logDebug, logWarning)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Delegation ()
import           Pos.Core (ProxySKHeavy)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.Delegation.Class (MonadDelegation)
import           Pos.Delegation.Configuration (HasDlgConfiguration)
import           Pos.Delegation.Logic (PskHeavyVerdict (..),
                     processProxySKHeavy)
import           Pos.Infra.Communication.Protocol (Message)
import           Pos.Infra.Communication.Relay (DataMsg)
import           Pos.Infra.StateLock (StateLock)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Util (HasLens')

-- Message constraints we need to be defined.
type DlgMessageConstraint
     = ( Message (DataMsg ProxySKHeavy)
       )

-- | This is a subset of 'WorkMode'.
type DlgListenerConstraint ctx m
     = ( MonadIO m
       , MonadUnliftIO m
       , MonadDelegation ctx m
       , MonadMask m
       , Mockable Delay m
       , Mockable CurrentTime m
       , MonadGState m
       , MonadBlockDBRead m
       , HasLens' ctx StateLock
       , HasLrcContext ctx
       , WithLogger m
       , DlgMessageConstraint
       , HasDlgConfiguration
       )

handlePsk :: (DlgListenerConstraint ctx m) => ProtocolMagic -> ProxySKHeavy -> m Bool
handlePsk pm pSk = do
    logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
    verdict <- processProxySKHeavy pm pSk
    logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
    case verdict of
        PHTipMismatch -> do
            -- We're probably updating state over epoch, so
            -- leaders can be calculated incorrectly. This is
            -- really weird and must not happen. We'll just retry.
            logWarning "Tip mismatch happened in delegation db!"
            handlePsk pm pSk
        PHAdded -> pure True
        PHRemoved -> pure True
        _ -> pure False
