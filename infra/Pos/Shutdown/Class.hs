{-# LANGUAGE TypeFamilies #-}

module Pos.Shutdown.Class
       ( HasShutdownContext(..)
       ) where

import           Universum

import           Pos.Shutdown.Types (ShutdownContext)

class HasShutdownContext ctx where
    shutdownContext :: Lens' ctx ShutdownContext
