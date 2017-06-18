module Pos.Shutdown.Types
       ( ShutdownContext (..)
       ) where

import           Control.Concurrent.STM (TBQueue)
import           Universum

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(TVar Bool)
    -- ^ If this flag is `True`, then workers should stop.
    , _shdnNotifyQueue :: !(TBQueue ())
    -- ^ A queue which is used to count how many workers have successfully
    -- terminated.
    }
