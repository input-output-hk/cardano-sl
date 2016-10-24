-- | Server part.

module Pos.Communication.Server
       ( serve
       ) where

import           Control.TimeWarp.Rpc           (Port, listen, Binding (AtPort))
import           Universum

import           Pos.Communication.Server.Block (blockListeners)
import           Pos.Communication.Server.Mpc   (mpcListeners)
import           Pos.Communication.Server.Tx    (txListeners)
import           Pos.WorkMode                   (WorkMode)

-- | Run server with all endpoints (i. e. listeners).
serve :: WorkMode m => Port -> m ()
serve port = listen (AtPort port) $ mconcat [blockListeners, mpcListeners, txListeners]
