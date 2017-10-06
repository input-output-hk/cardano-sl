module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Universum

import           Control.Monad.Catch (MonadMask)
import           Ether.Internal      (HasLens (..))
import           Mockable            (MonadMockable)
import           System.Wlog         (WithLogger)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.DB.Class        (MonadDB, MonadGState)
import           Pos.Lrc.Context     (LrcContext)
import           Pos.Reporting       (MonadReporting)
import           Pos.StateLock       (StateLock)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Update.Context  (UpdateContext)
import           Pos.Update.Params   (UpdateParams)

type UpdateMode ctx m
    = ( WithLogger m
      , MonadMockable m
      , MonadIO m
      , MonadMask m
      , MonadGState m
      , MonadDB m
      , MonadReader ctx m
      , HasLens UpdateContext ctx UpdateContext
      , HasLens LrcContext ctx LrcContext
      , HasLens UpdateParams ctx UpdateParams
      , HasLens StateLock ctx StateLock
      , HasConfiguration
      , HasUpdateConfiguration
      , MonadReporting ctx m
      )
