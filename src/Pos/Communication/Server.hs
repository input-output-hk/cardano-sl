-- | Server part.

module Pos.Communication.Server
       ( serve
       ) where

import           Control.TimeWarp.Rpc           (listen)
import           Universum

import           Pos.Communication.Server.Block (blockListeners)
import           Pos.Communication.Server.Mpc   (mpcListeners)
import           Pos.Communication.Server.Tx    (txListeners)
import           Pos.WorkMode                   (WorkMode)

-- TODO: not hardcode port.
serve :: WorkMode m => m ()
serve = listen 322 $ mconcat [blockListeners, mpcListeners, txListeners]
