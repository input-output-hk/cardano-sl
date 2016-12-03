{-# LANGUAGE FlexibleContexts #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       ) where

import           Control.TimeWarp.Timed (currentTime, for, fork, sleepForever, wait)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (logError, logInfo)
import           Universum

import           Pos.DHT                (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.State              (initFirstSlot)
import           Pos.Types              (Address, Coin, Timestamp (Timestamp), Tx (..),
                                         TxId, txF)
import           Pos.Util               (inAssertMode)
import           Pos.Wallet             (makePubKeyTx)
import           Pos.Worker             (runWorkers)
import           Pos.WorkMode           (NodeContext (..), WorkMode, getNodeContext,
                                         ncPublicKey)
-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, WorkMode ssc m) => [m ()] -> m ()
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
