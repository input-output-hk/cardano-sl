{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , allStubListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (untag)
import           System.Wlog                       (LoggerName, WithLogger)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Listeners       (blockListeners, blockStubListeners)
import           Pos.Communication.Protocol        (Listener, mapListener')
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (convWithTimeLimit,
                                                    modifyListenerLogger,
                                                    sendActionsWithTimeLimit, withWaitLog,
                                                    withWaitLogConvL)
import           Pos.Constants                     (networkReceiveTimeout)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp.Listeners                 (txListeners, txStubListeners)
import           Pos.Update                        (usListeners, usStubListeners)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, WorkMode ssc m)
    => [Listener m]
allListeners =
    map addWaitLogging $
    map (addTimeout networkReceiveTimeout) $
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "update") usListeners
        ]
  where
    addWaitLogging = mapListener' withWaitLog withWaitLogConvL identity
    addTimeout timeout = mapListener' (sendActionsWithTimeLimit timeout) (convWithTimeLimit timeout) identity

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, WithLogger m) => Proxy ssc -> [Listener m]
allStubListeners p =
    concat
        [ blockStubListeners p
        , sscStubListeners p
        , txStubListeners p
        , delegationStubListeners
        , usStubListeners
        ]

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
