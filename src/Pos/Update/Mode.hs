{-# LANGUAGE ConstraintKinds #-}

module Pos.Update.Mode
       ( UpdateMode
       ) where

import           Control.Monad.Catch     (MonadMask)

import           Pos.Communication.Relay (MonadRelayMem)
import           Pos.DB.Class            (MonadDB)
import           Pos.DB.Limits           (MonadDBLimits)
import           Pos.Launcher.Param      (NodeParams)
import           Pos.Lrc.Context         (LrcContext)
import           Pos.Update.Context      (UpdateContext)
import           Pos.Update.MemState     (MonadUSMem)
import           Pos.Util.Context        (HasContext)
import           Pos.WorkMode            (MinWorkMode)

type UpdateMode m
    = ( MinWorkMode m
      , MonadMask m
      , MonadDB m
      , MonadDBLimits m
      , MonadRelayMem m
      , MonadUSMem m
      , HasContext UpdateContext m
      , HasContext LrcContext m
      , HasContext NodeParams m
      )
