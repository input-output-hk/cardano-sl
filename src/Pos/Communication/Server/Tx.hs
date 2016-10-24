-- | Server which handles transactions.

module Pos.Communication.Server.Tx
       ( txListeners
       ) where

import           Control.TimeWarp.Logging (logInfo)
import           Control.TimeWarp.Rpc     (Listener (..))
import           Formatting               (build, sformat, (%))
import           Universum

import           Pos.Communication.Types  (ResponseMode, SendTx (..))
import           Pos.State                (processTx)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners :: WorkMode m => [Listener m]
txListeners =
    [ Listener handleTx
    ]

handleTx :: ResponseMode m => SendTx -> m ()
handleTx (SendTx tx) =
    whenM (processTx tx) $
    logInfo (sformat ("Transaction has been added to storage: "%build) tx)
