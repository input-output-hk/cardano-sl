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
import           Pos.Communication.Relay     (relayListeners)
import           Pos.Communication.Util      (wrapListener)
import           Pos.Delegation.Listeners    (delegationRelays)
import           Pos.Ssc.Class               (SscListenersClass (..), SscWorkersClass)
import           Pos.Txp                     (txRelays)
import           Pos.Update                  (usRelays)
import           Pos.WorkMode.Class          (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc m)
    => MkListeners m
allListeners = mconcat
        [ modifier "block"       $ blockListeners
        , modifier "ssc"         $ relayListeners (untag sscRelays)
        , modifier "tx"          $ relayListeners txRelays
        , modifier "delegation"  $ relayListeners delegationRelays
        , modifier "update"      $ relayListeners usRelays
        ]
  where
    modifier lname mkL = mkL { mkListeners = mkListeners' }
      where
        mkListeners' v p = do
            ls <- mkListeners mkL v p
            let f = wrapListener (serverLoggerName <> lname)
            pure $ map f ls

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
