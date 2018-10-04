{-# LANGUAGE DataKinds #-}

-- | Constraints for LRC; a restricted version of `WorkMode`.

module Pos.DB.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.DB.Lrc.Context (HasLrcContext)
import           Pos.Util.Wlog (WithLogger)

-- | Set of constraints used by LRC.
type LrcMode ctx m
     = ( WithLogger m
       , MonadMask m
       , MonadGState m
       , MonadDB m
       , MonadIO m
       , MonadUnliftIO m
       , MonadReader ctx m
       , HasLrcContext ctx
       )
