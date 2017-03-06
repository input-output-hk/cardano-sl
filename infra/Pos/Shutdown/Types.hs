module Pos.Shutdown.Types
       ( ShutdownContext (..)
       ) where

import           Control.Concurrent.STM (TBQueue)
import qualified Control.Concurrent.STM as STM
import           Universum

data ShutdownContext = ShutdownContext
    { _shdnIsTriggered :: !(STM.TVar Bool)
    , _shdnNotifyQueue :: !(TBQueue ())
    }
