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
import           Pos.Communication.Protocol (Message)
import           Pos.Communication.Relay (DataMsg)
import           Pos.Core (ProxySKHeavy, HasGenesisBlockVersionData, HasProtocolMagic)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.Delegation.Class (MonadDelegation)
import           Pos.Delegation.Configuration (HasDlgConfiguration)
import           Pos.Delegation.Logic (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.StateLock (StateLock)
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

handlePsk :: (HasProtocolMagic, HasGenesisBlockVersionData, DlgListenerConstraint ctx m) => ProxySKHeavy -> m Bool
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
