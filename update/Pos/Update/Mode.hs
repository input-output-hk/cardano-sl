{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Mockable (MonadMockable)
import           System.Wlog (WithLogger)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Update ()
import           Pos.Core.Configuration ()
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
import           Pos.Util.Util (HasLens (..))

type UpdateMode ctx m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , MonadUnliftIO m
      , MonadMask m
      , MonadGState m
      , MonadDB m
      , MonadReader ctx m
      , HasLrcContext ctx
      , HasLens UpdateContext ctx UpdateContext
      , HasLens UpdateParams ctx UpdateParams
      , HasLens StateLock ctx StateLock
      , HasShutdownContext ctx
      , HasUpdateConfiguration
      , MonadReporting m
      , MonadRecoveryInfo m
      , MonadSlots ctx m
      )
