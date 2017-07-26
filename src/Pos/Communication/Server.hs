-- | Server part.

{-# LANGUAGE RankNTypes #-}

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
import           Pos.Communication.Protocol  (MkListeners (..), EnqueueMsg)
import           Pos.Communication.Relay     (relayListeners)
import           Pos.Communication.Util      (wrapListener)
import           Pos.Delegation.Listeners    (delegationRelays)
import           Pos.Network.Types           (Topology)
import           Pos.Ssc.Class               (SscListenersClass (..), SscWorkersClass)
import           Pos.Txp                     (txRelays)
import           Pos.Update                  (usRelays)
import           Pos.WorkMode.Class          (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc ctx m)
    => Topology -> EnqueueMsg m -> MkListeners m
allListeners topology enqueue = mconcat
        -- TODO blockListeners should use 'enqueue' rather than its own
        -- block retrieval queue, no?
        [ modifier "block"       $ blockListeners topology
        , modifier "ssc"         $ relayListeners enqueue (untag sscRelays)
        , modifier "tx"          $ relayListeners enqueue txRelays
        , modifier "delegation"  $ relayListeners enqueue delegationRelays
        , modifier "update"      $ relayListeners enqueue usRelays
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
