{-# LANGUAGE TemplateHaskell #-}

module Pos.Shutdown.Types
       ( ShutdownContext (..)
       , shdnIsTriggered
       , shdnNotifyQueue
       ) where

import           Universum

import           Control.Concurrent.STM (TBQueue)
import           Control.Lens           (makeLenses)

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    , _shdnNotifyQueue :: !(TBQueue ())
    -- ^ A queue which is used to count how many workers have successfully
    -- terminated.
    }

makeLenses ''ShutdownContext
