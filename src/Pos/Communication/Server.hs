-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , allStubListeners
       , serverLoggerName
       ) where

import           Data.Tagged                       (Tagged, proxy, unproxy, untag)
import           System.Wlog                       (LoggerName, WithLogger)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Listeners       (blockListeners, blockStubListeners)
import           Pos.Communication.Protocol        (ListenerSpec (..), OutSpecs, NodeId)
import           Pos.Communication.Util            (wrapListener)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class                     (SscHelpersClass (..),
                                                    SscListenersClass (..),
                                                    SscWorkersClass)
import           Pos.Txp                           (txListeners, txStubListeners)
import           Pos.Update                        (usListeners, usStubListeners)
import           Pos.Util                          (mconcatPair)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc m)
    => m (Set NodeId) -> m ([ListenerSpec m], OutSpecs)
allListeners getPeers = mconcatPair <$> sequence
        [ modifier "block"       <$> blockListeners
        , modifier "ssc" . untag <$> sscListeners
        , modifier "tx"          <$> txListeners
        , modifier "delegation"  <$> pure (delegationListeners getPeers)
        , modifier "update"      <$> usListeners
        ]
  where
    modifier lname = over _1 (map pModifier)
      where
        pModifier (ListenerSpec h spec) =
            ListenerSpec (\vI -> wrapListener (serverLoggerName <> lname) $ h vI) spec

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, WithLogger m, SscHelpersClass ssc)
    => Tagged ssc ([ListenerSpec m], OutSpecs)
allStubListeners = unproxy $ \sscProxy ->
    mconcatPair
        [ proxy blockStubListeners sscProxy
        , proxy sscStubListeners sscProxy
        , txStubListeners
        , delegationStubListeners
        , usStubListeners
        ]

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
