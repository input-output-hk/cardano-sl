{-# LANGUAGE TemplateHaskell #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , initLrc
       ) where

import           Control.Concurrent.MVar     (putMVar)
import           Control.Concurrent.STM.TVar (writeTVar)
import           Development.GitRev          (gitBranch, gitHash)
import           Formatting                  (build, int, sformat, (%))
import           Mockable                    (currentTime, delay, fork, sleepForever)
import           Node                        (SendActions)
import           System.Wlog                 (logError, logInfo)
import           Universum

import           Pos.Communication           (BiP)
import           Pos.Context                 (NodeContext (..), getNodeContext,
                                              ncPubKeyAddress, ncPublicKey)
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.Lrc                  as LrcDB
import           Pos.DHT.Model               (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.Types                   (Timestamp (Timestamp), addressHash)
import           Pos.Util                    (inAssertMode, waitRandomInterval)
import           Pos.Util.TimeWarp           (sec)
import           Pos.Worker                  (runWorkers)
import           Pos.WorkMode                (WorkMode)

-- | Run full node in any WorkMode.
runNode
    :: (SscConstraint ssc, WorkMode ssc m)
    => [SendActions BiP m -> m ()]
    -> SendActions BiP m
    -> m ()
runNode plugins sendActions = do
    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    addr <- ncPubKeyAddress <$> getNodeContext
    let pkHash = addressHash pk
    logInfo $ sformat ("My public key is: "%build%
                       ", address: "%build%
                       ", pk hash: "%build) pk addr pkHash
    () <$ fork waitForPeers
    initSemaphore
    initLrc
    waitSystemStart
    runWorkers sendActions
    mapM_ (fork . ($ sendActions)) plugins
    sleepForever

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    let waitPeriod = start - cur
    logInfo $ sformat ("Waiting "%int%" seconds for system start") $
        waitPeriod `div` sec 1
    when (cur < start) $ delay (start - cur)

-- | Try to discover peers repeatedly until at least one live peer is found
waitForPeers :: WorkMode ssc m => m ()
waitForPeers = discoverPeers DHTFull >>= \case
    ps@(_:_) -> () <$ logInfo (sformat ("Known peers: "%build) ps)
    []       -> logInfo "Couldn't connect to any peer, trying again..." >>
                waitRandomInterval (sec 3) (sec 10) >>
                waitForPeers

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- ncBlkSemaphore <$> getNodeContext
    unlessM
        (liftIO $ isEmptyMVar semaphore)
        (logError "ncBlkSemaphore is not empty at the very beginning")
    tip <- GS.getTip
    liftIO $ putMVar semaphore tip

initLrc :: WorkMode ssc m => m ()
initLrc = do
    lrcSync <- ncLrcSync <$> getNodeContext
    atomically . writeTVar lrcSync . (True,) =<< LrcDB.getEpoch
