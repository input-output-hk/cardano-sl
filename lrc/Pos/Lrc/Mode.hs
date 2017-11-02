{-# LANGUAGE DataKinds #-}

-- | Set of constraints used by LRC.

module Pos.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import           Mockable        (Async, Bracket, Concurrently, Delay, Mockables)
import           System.Wlog     (WithLogger)

import           Pos.Core        (HasConfiguration)
import           Pos.DB.Class    (MonadDB, MonadGState)
import           Pos.Lrc.Context (HasLrcContext)

-- | Set of constraints used by LRC.
type LrcMode ctx m
     = ( WithLogger m
       , MonadMask m
       , MonadGState m
       , MonadDB m
       , MonadIO m
       , Mockables m [Async, Bracket, Concurrently, Delay]
                     -- ^ alphabet for the youngest haskellers
       , MonadReader ctx m
       , HasLrcContext ctx
       , HasConfiguration
       )
