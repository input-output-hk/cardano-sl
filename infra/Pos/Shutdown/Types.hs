module Pos.Shutdown.Types
       ( ShutdownContext (..)
       ) where

import           Control.Concurrent.STM (TBQueue)
import           Universum

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    , _shdnNotifyQueue :: !(TBQueue ())
    }
