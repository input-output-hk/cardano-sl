-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , sscRelays
       , txRelays
       , delegationRelays
       , usRelays
       ) where

import           Universum

import           Data.Tagged                 (untag)
import           System.Wlog                 (LoggerName)

import           Pos.Binary.Communication    ()
import           Pos.Block.Network.Listeners (blockListeners)
import           Pos.Communication.Protocol  (MkListeners (..))
import           Pos.Communication.Relay     (relayListeners, RelayContext)
import           Pos.Communication.Util      (wrapListener)
import           Pos.Delegation.Listeners    (delegationRelays)
import           Pos.Ssc.Class               (SscListenersClass (..), SscWorkersClass)
import           Pos.Txp                     (txRelays)
import           Pos.Update                  (usRelays)
import           Pos.WorkMode.Class          (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc m)
    => RelayContext m -> MkListeners m
allListeners relayContext = mconcat
        [ modifier "block"       $ blockListeners
        , modifier "ssc"         $ relayListeners relayContext (untag sscRelays)
        , modifier "tx"          $ relayListeners relayContext txRelays
        , modifier "delegation"  $ relayListeners relayContext (delegationRelays relayContext)
        , modifier "update"      $ relayListeners relayContext usRelays
        ]
  where
    modifier lname mkL = mkL { mkListeners = mkListeners' }
      where
        mkListeners' v p =
            let ls = mkListeners mkL v p
                f = wrapListener (serverLoggerName <> lname)
            in  map f ls

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
