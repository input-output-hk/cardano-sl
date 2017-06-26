{-# LANGUAGE TypeFamilies #-}

module Pos.Shutdown.Class
       ( MonadShutdownMem
       , askShutdownMem
       ) where

import           Universum

import           EtherCompat
import           Pos.Shutdown.Types (ShutdownContext)

type MonadShutdownMem ctx m = (MonadReader ctx m, HasLens ShutdownContext ctx ShutdownContext)

askShutdownMem :: MonadShutdownMem ctx m => m ShutdownContext
askShutdownMem = view (lensOf @ShutdownContext)
