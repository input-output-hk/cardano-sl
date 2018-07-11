{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic.

module Pos.Listener.Delegation
       ( handlePsk
       , DlgListenerConstraint
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           Mockable (CurrentTime, Delay, Mockable)
import           UnliftIO (MonadUnliftIO)

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
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logWarning)

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
       , DlgMessageConstraint
       , HasDlgConfiguration
       )

handlePsk
    :: (DlgListenerConstraint ctx m)
    => TraceNamed m
    -> ProtocolMagic
    -> ProxySKHeavy
    -> m Bool
handlePsk logTrace pm pSk = do
    logDebug logTrace $ sformat ("Got request to handle heavyweight psk: "%build) pSk
    verdict <- processProxySKHeavy pm pSk
    logDebug logTrace $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
    case verdict of
        PHTipMismatch -> do
            -- We're probably updating state over epoch, so
            -- leaders can be calculated incorrectly. This is
            -- really weird and must not happen. We'll just retry.
            logWarning logTrace "Tip mismatch happened in delegation db!"
            handlePsk logTrace pm pSk
        PHAdded -> pure True
        PHRemoved -> pure True
        _ -> pure False
