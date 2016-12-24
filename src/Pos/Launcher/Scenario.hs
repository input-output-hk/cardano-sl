{-# LANGUAGE FlexibleContexts #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Control.TimeWarp.Timed  (currentTime, for, fork, sleepForever, wait)
import           Formatting              (build, sformat, (%))
import           System.Wlog             (logError, logInfo)
import           Universum

import           Pos.Constants           (k)
import           Pos.Context             (NodeContext (..), getNodeContext, ncPublicKey,
                                          putLeaders, putRichmen)
import qualified Pos.DB                  as DB
import           Pos.DB.Utxo             (getTip)
import           Pos.DHT.Model           (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Slotting            (getCurrentSlot)
import           Pos.Ssc.Class           (SscConstraint)
import           Pos.State               (initFirstSlot)
import           Pos.Types               (SlotId (..), Timestamp (Timestamp))
import           Pos.Util                (inAssertMode)
import           Pos.Worker              (runWorkers)
import           Pos.WorkMode            (WorkMode)

-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, WorkMode ssc m) => [m ()] -> m ()
runNode plugins = do
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    logInfo $ sformat ("My public key is: "%build) pk
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    initSemaphore
    initLrc
--    initSsc
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

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- ncBlkSemaphore <$> getNodeContext
    unlessM
        (liftIO $ isEmptyMVar semaphore)
        (logError "ncBlkSemaphore is not empty at the very beginning")
    tip <- getTip
    liftIO $ putMVar semaphore tip

initLrc :: WorkMode ssc m => m ()
initLrc = do
    (epochIndex, leaders, richmen) <- DB.getLrc
    SlotId {..} <- getCurrentSlot
    when (siSlot < k && siEpoch == epochIndex) $ do
        putLeaders leaders
        putRichmen richmen

-- initSsc :: WorkMode ssc m => m ()
-- initSsc = do
--     tip <- getTip
--     gs <- sscLoadGlobalState tip
--     setGlobalState gs
