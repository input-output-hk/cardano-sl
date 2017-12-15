{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)
import           Data.Tagged (Tagged)
import           Ether.Internal (HasLens (..))
import           Mockable (MonadMockable)
import           Node.Message.Class (Message)
import           System.Wlog (WithLogger)

import           Pos.Binary.Update ()
import           Pos.Communication.Limits.Types (MessageLimited)
import qualified Pos.Communication.Relay as Relay
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Update (UpId, UpdateProposal, UpdateVote)
import           Pos.Crypto (Hash, PublicKey)
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown.Class (HasShutdownContext)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.StateLock (StateLock)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Update.Context (UpdateContext)
import           Pos.Update.Params (UpdateParams)

type UpdateMode ctx m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , MonadMask m
      , MonadGState m
      , MonadDB m
      , MonadReader ctx m
      , HasLrcContext ctx
      , HasLens UpdateContext ctx UpdateContext
      , HasLens UpdateParams ctx UpdateParams
      , HasLens StateLock ctx StateLock
      , HasShutdownContext ctx
      , HasConfiguration
      , HasUpdateConfiguration
      , MonadReporting ctx m
      , MonadRecoveryInfo m
      , MonadSlots ctx m
      -- TODO: Wouldn't it be /nice/ if we had a type synonym for
      -- @(UpdateProposal, [UpdateVote])@
      --
      -- NB. We could use 'Each' here but then GHC complains about
      --
      --     update/Pos/Update/Network/Listeners.hs:1:1: error:
      --         solveWanteds: too many iterations (limit = 4)
      --
      , Message $
          Relay.InvOrData
             (Tagged (UpdateProposal, [UpdateVote]) (Hash UpdateProposal))
             (UpdateProposal, [UpdateVote])
      , Message $
          Relay.InvOrData
             (Tagged UpdateVote (UpId, PublicKey, Bool))
             UpdateVote
      , Message $
          Relay.ReqOrRes
             (Tagged (UpdateProposal, [UpdateVote]) (Hash UpdateProposal))
      , Message $
          Relay.ReqOrRes
             (Tagged UpdateVote (UpId, PublicKey, Bool))
      , Message $
          Relay.ReqMsg
             (Tagged (UpdateProposal, [UpdateVote]) (Hash UpdateProposal))
      , Message $
          Relay.ReqMsg
             (Tagged UpdateVote (UpId, PublicKey, Bool))
      , MessageLimited (Relay.DataMsg UpdateVote) m
      , MessageLimited (Relay.DataMsg (UpdateProposal, [UpdateVote])) m
      )
