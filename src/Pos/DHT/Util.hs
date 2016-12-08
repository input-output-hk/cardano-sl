module Pos.DHT.Util (joinNetworkNoThrow
) where

import           Control.Monad.Catch             (MonadCatch, catch, throwM)
import           Formatting                      (build, sformat, (%))
import           System.Wlog                     (WithLogger, logInfo)
import           Universum

import           Pos.DHT.Types                   (DHTNode)
import           Pos.DHT.Class                   (MonadDHT,
                                                  DHTException (AllPeersUnavailable),
                                                  joinNetwork)

-- | Join distributed network without throwing 'AllPeersUnavailable' exception.
joinNetworkNoThrow :: ( MonadDHT   m
                      , MonadCatch m
                      , WithLogger m
                      ) => [DHTNode] -> m ()
joinNetworkNoThrow peers = joinNetwork peers `catch` handleJoinE
  where
    handleJoinE AllPeersUnavailable =
        logInfo $ sformat ("Not connected to any of peers " % build) peers
    handleJoinE e = throwM e
