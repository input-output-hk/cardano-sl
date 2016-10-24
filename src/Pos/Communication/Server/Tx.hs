-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Rpc    (Listener (..))
import           Universum

import           Pos.Communication.Types (ResponseMode, SendTx (..))
import           Pos.WorkMode            (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: WorkMode m => [Listener m]
txListeners =
    [ Listener handleTx
    ]

handleTx :: ResponseMode m => SendTx -> m ()
handleTx (SendTx _) = notImplemented
