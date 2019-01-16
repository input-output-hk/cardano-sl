{-# LANGUAGE TemplateHaskell #-}

module Pos.Infra.Shutdown.Types
       ( ShutdownContext (..)
       , shdnIsTriggered, shdnFInjects
       ) where

import           System.Exit (ExitCode)
import           Universum

import           Control.Lens (makeLenses)
import           Pos.Infra.InjectFail (FInjects)

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar (Maybe ExitCode))
    -- ^ If this flag is `Just`, then workers should stop.
    , _shdnFInjects    :: !(FInjects IO)
    }

makeLenses ''ShutdownContext
