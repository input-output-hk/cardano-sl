module Pos.DHT.Model.Util
       ( joinNetworkNoThrow
       ) where

import           Formatting          (build, sformat, (%))
import           Mockable            (Catch, Mockable, Throw, catch, throw)
import           System.Wlog         (WithLogger, logInfo)
import           Universum           hiding (catch)

import           Pos.DHT.Model.Class (DHTException (AllPeersUnavailable), MonadDHT,
                                      joinNetwork)
import           Pos.DHT.Model.Types (DHTNode)

-- | Join distributed network without throwing 'AllPeersUnavailable' exception.
joinNetworkNoThrow
    :: (MonadDHT m, Mockable Catch m, Mockable Throw m, WithLogger m)
    => [DHTNode] -> m ()
joinNetworkNoThrow peers = joinNetwork peers `catch` handleJoinE
  where
    handleJoinE AllPeersUnavailable =
        logInfo $ sformat ("Not connected to any of peers " % build) peers
    handleJoinE e = throw e
