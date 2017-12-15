{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationRelays
       ) where

import           Universum

import           Formatting (build, sformat, shown, (%))
import           Mockable (CurrentTime, Delay, Mockable)
import           System.Wlog (WithLogger, logDebug, logWarning)

import           Pos.Binary.Delegation ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Protocol (Message, MsgType (..))
import           Pos.Communication.Relay (DataMsg, DataParams (..), Relay (..))
import           Pos.Core (ProxySKHeavy)
import           Pos.DB.Class (MonadBlockDBRead, MonadGState)
import           Pos.Delegation.Class (MonadDelegation)
import           Pos.Delegation.Configuration (HasDlgConfiguration)
import           Pos.Delegation.Logic (PskHeavyVerdict (..), processProxySKHeavy)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.StateLock (StateLock)
import           Pos.Util (HasLens')

-- Message constraints we need to be defined.
type DlgMessageConstraint m
     = ( Message (DataMsg ProxySKHeavy)
       , MessageLimited (DataMsg ProxySKHeavy) m
       )

-- | This is a subset of 'WorkMode'.
type DlgListenerConstraint ctx m
     = ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , Mockable Delay m
       , Mockable CurrentTime m
       , MonadGState m
       , MonadBlockDBRead m
       , HasLens' ctx StateLock
       , HasLrcContext ctx
       , WithLogger m
       , DlgMessageConstraint m
       , HasDlgConfiguration)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall ctx m. DlgListenerConstraint ctx m
    => [Relay m]
delegationRelays = [ pskHeavyRelay ]

pskHeavyRelay
    :: forall ctx m . DlgListenerConstraint ctx m
    => Relay m
pskHeavyRelay = Data $ DataParams MsgTransaction $ \_ _ -> handlePsk
  where
    handlePsk :: DlgListenerConstraint ctx m => ProxySKHeavy -> m Bool
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
