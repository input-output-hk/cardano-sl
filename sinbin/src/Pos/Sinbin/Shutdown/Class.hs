{-# LANGUAGE TypeFamilies #-}

module Pos.Sinbin.Shutdown.Class
       ( HasShutdownContext(..)
       ) where

import           Universum

import           Control.Lens.Lens (lens)
import           Pos.Sinbin.Shutdown.Types (ShutdownContext)

class HasShutdownContext ctx where
    shutdownContext :: Lens' ctx ShutdownContext

instance HasShutdownContext ShutdownContext where
    shutdownContext = lens identity (\_ x -> x)
