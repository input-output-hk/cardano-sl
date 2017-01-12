-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , initLrc
       ) where

import           Control.Concurrent.MVar     (putMVar)
import           Formatting                  (build, sformat, (%))
import           Mockable                    (currentTime, delay, fork, sleepForever)
import           Node                        (SendActions)
import           System.Wlog                 (logError, logInfo)
import           Universum

import           Control.Concurrent.STM.TVar (writeTVar)
import           Pos.Communication           (BiP)
import           Pos.Context                 (NodeContext (..), getNodeContext,
                                              ncPubKeyAddress, ncPublicKey)
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.Lrc                  as LrcDB
import           Pos.DHT.Model               (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.Types                   (Timestamp (Timestamp), addressHash)
import           Pos.Util                    (inAssertMode)
import           Pos.Worker                  (runWorkers)
import           Pos.WorkMode                (NewWorkMode)

-- | Run full node in any WorkMode.
runNode
    :: (SscConstraint ssc, NewWorkMode ssc m)
    => [SendActions BiP m -> m ()]
    -> SendActions BiP m
    -> m ()
runNode plugins sendActions = do
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    addr <- ncPubKeyAddress <$> getNodeContext
    let pkHash = addressHash pk
    logInfo $ sformat ("My public key is: "%build%
                       ", address: "%build%
                       ", pk hash: "%build) pk addr pkHash
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    initSemaphore
    initLrc
    waitSystemStart
    runWorkers sendActions
    mapM_ (fork . ($ sendActions)) plugins
    sleepForever

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: NewWorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ delay (start - cur)

initSemaphore :: (NewWorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- ncBlkSemaphore <$> getNodeContext
    unlessM
        (liftIO $ isEmptyMVar semaphore)
        (logError "ncBlkSemaphore is not empty at the very beginning")
    tip <- GS.getTip
    liftIO $ putMVar semaphore tip

initLrc :: NewWorkMode ssc m => m ()
initLrc = do
    lrcSync <- ncLrcSync <$> getNodeContext
    atomically . writeTVar lrcSync . (True,) =<< LrcDB.getEpoch
