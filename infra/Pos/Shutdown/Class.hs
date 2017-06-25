{-# LANGUAGE TypeFamilies #-}

module Pos.Shutdown.Class
       ( MonadShutdownMem
       , askShutdownMem
       ) where

import           EtherCompat
import           Pos.Shutdown.Types (ShutdownContext)

type MonadShutdownMem ctx m = MonadCtx ctx ShutdownContext ShutdownContext m

askShutdownMem :: MonadShutdownMem ctx m => m ShutdownContext
askShutdownMem = askCtx @ShutdownContext
