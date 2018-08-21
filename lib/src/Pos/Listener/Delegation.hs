{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic.

module Pos.Listener.Delegation
       ( handlePsk
       , DlgListenerConstraint
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Delegation (HasDlgConfiguration, MonadDelegation)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.DB.Delegation (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.DB.GState.Lock (StateLock)
import           Pos.DB.Lrc (HasLrcContext)
import           Pos.Infra.Communication.Protocol (Message)
import           Pos.Infra.Communication.Relay (DataMsg)
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
