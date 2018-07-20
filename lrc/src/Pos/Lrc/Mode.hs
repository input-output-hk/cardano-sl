{-# LANGUAGE DataKinds #-}

-- | Constraints for LRC; a restricted version of `WorkMode`.

module Pos.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import           System.Wlog (WithLogger)
import           UnliftIO (MonadUnliftIO)

import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.Lrc.Context (HasLrcContext)

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
