{-# LANGUAGE TemplateHaskell #-}

module Pos.Infra.Shutdown.Types
       ( ShutdownContext (..)
       , shdnIsTriggered, shdnFInjects
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Pos.Infra.InjectFail (FInjects)

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    , _shdnFInjects    :: !FInjects
    -- ^ If this flag is `True`, then workers should stop.
    }

makeLenses ''ShutdownContext
