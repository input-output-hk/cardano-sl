{-# LANGUAGE FlexibleContexts #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       ) where

import           Control.TimeWarp.Timed (currentTime, for, fork, sleepForever, wait)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (logInfo)
import           Universum

import           Pos.Context            (NodeContext (..), getNodeContext, ncPublicKey)
import           Pos.DHT                (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Security           (SecurityWorkersClass)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.State              (initFirstSlot)
import           Pos.Types              (Timestamp (Timestamp))
import           Pos.Util               (inAssertMode)
import           Pos.Worker             (runWorkers)
import           Pos.WorkMode           (WorkMode)

-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, SecurityWorkersClass ssc, WorkMode ssc m) => [m ()] -> m ()
runNode plugins = do
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    initFirstSlot
    waitSystemStart
    runWorkers
    mapM_ fork plugins
    sleepForever

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ wait (for (start - cur))
