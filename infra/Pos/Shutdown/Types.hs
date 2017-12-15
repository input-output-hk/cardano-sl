{-# LANGUAGE TemplateHaskell #-}

module Pos.Shutdown.Types
       ( ShutdownContext (..)
       , shdnIsTriggered
       ) where

import           Universum

import           Control.Lens (makeLenses)

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    }

makeLenses ''ShutdownContext
