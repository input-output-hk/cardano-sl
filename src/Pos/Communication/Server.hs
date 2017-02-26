
-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , allStubListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (Tagged, proxy, unproxy, untag)
import           System.Wlog                       (LoggerName, WithLogger)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Listeners       (blockListeners, blockStubListeners)
import           Pos.Communication.Protocol        (ListenerSpec (..), OutSpecs)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (wrapListener)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Helpers             (SscHelpersClass (..))
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp                           (txListeners, txStubListeners)
import           Pos.Update                        (usListeners, usStubListeners)
import           Pos.Util                          (mconcatPair)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, WorkMode ssc m)
    => ([ListenerSpec m], OutSpecs)
allListeners = mconcatPair
        [ modifier "block"      $ blockListeners
        , modifier "ssc"        $ untag sscListeners
        , modifier "tx"         $ txListeners
        , modifier "delegation" $ delegationListeners
        , modifier "update"     $ usListeners
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
