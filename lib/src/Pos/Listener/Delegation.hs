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

import           Pos.Chain.Delegation (HasDlgConfiguration, MonadDelegation,
                     ProxySKHeavy)
import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.DB.Delegation (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.DB.Lrc (HasLrcContext)
import           Pos.Infra.Communication.Protocol (Message)
import           Pos.Infra.Communication.Relay (DataMsg)
import           Pos.Infra.StateLock (StateLock)
import           Pos.Util (HasLens')
import           Pos.Util.Wlog (WithLogger, logDebug, logWarning)

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
       , WithLogger m
       , DlgMessageConstraint
       , HasDlgConfiguration
       )

handlePsk
    :: (DlgListenerConstraint ctx m) => Genesis.Config -> ProxySKHeavy -> m Bool
handlePsk genesisConfig pSk = do
    logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
    verdict <- processProxySKHeavy genesisConfig pSk
    logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
    case verdict of
        PHTipMismatch -> do
            -- We're probably updating state over epoch, so
            -- leaders can be calculated incorrectly. This is
            -- really weird and must not happen. We'll just retry.
            logWarning "Tip mismatch happened in delegation db!"
            handlePsk genesisConfig pSk
        PHAdded -> pure True
        PHRemoved -> pure True
        _ -> pure False
