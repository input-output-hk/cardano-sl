{-# LANGUAGE DataKinds #-}

-- | Set of constraints used by LRC.

module Pos.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import qualified Ether
import           Mockable        (Async, Concurrently, Delay, Mockables)
import           System.Wlog     (WithLogger)

import           Pos.DB.Class    (MonadDB, MonadGState, MonadRealDB)
import           Pos.Lrc.Context (LrcContext)

-- | Set of constraints used by LRC.
type LrcMode ssc m
     = ( WithLogger m
       , MonadMask m
       , MonadGState m
       , MonadDB m
       , MonadRealDB m
       , Mockables m [Async, Concurrently, Delay]
       , Ether.MonadReader' LrcContext m
       )
