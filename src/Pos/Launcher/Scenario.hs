{-# LANGUAGE TemplateHaskell #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , initLrc
       , runNode'
       ) where

import           Control.Concurrent.MVar     (putMVar)
import           Control.Concurrent.STM.TVar (writeTVar)
import           Data.Default                (def)
import           Development.GitRev          (gitBranch, gitHash)
import           Formatting                  (build, sformat, (%))
import           Mockable                    (fork)
import           System.Wlog                 (logError, logInfo)
import           Universum

import           Pos.Communication           (ActionSpec (..), OutSpecs, WorkerSpec,
                                              wrapActionSpec)
import           Pos.Context                 (NodeContext (..), getNodeContext,
                                              ncPubKeyAddress, ncPublicKey)
import qualified Pos.DB.GState               as GS
import qualified Pos.DB.Lrc                  as LrcDB
import           Pos.Delegation.Logic        (initDelegation)
import           Pos.DHT.Model               (discoverPeers)
import           Pos.Slotting                (getCurrentSlot, waitSystemStart)
import           Pos.Ssc.Class               (SscConstraint)
import           Pos.Types                   (SlotId (..), addressHash)
import           Pos.Update                  (MemState (..), askUSMemVar, mvState)
import           Pos.Util                    (inAssertMode, waitRandomInterval)
import           Pos.Util.Shutdown           (waitForWorkers)
import           Pos.Util.TimeWarp           (sec)
import           Pos.Worker                  (allWorkers, allWorkersCount)
import           Pos.WorkMode                (WorkMode)

-- | Run full node in any WorkMode.
runNode'
    :: (SscConstraint ssc, WorkMode ssc m)
    => [WorkerSpec m]
    -> WorkerSpec m
runNode' plugins' = ActionSpec $ \vI sendActions -> do
    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    addr <- ncPubKeyAddress <$> getNodeContext
    let pkHash = addressHash pk
    logInfo $ sformat ("My public key is: "%build%
                       ", address: "%build%
                       ", pk hash: "%build) pk addr pkHash
    () <$ fork waitForPeers
    initDelegation
    initLrc
    initUSMemState
    initSemaphore
    waitSystemStart
    let unpackPlugin (ActionSpec action) = action vI sendActions
    mapM_ (fork . unpackPlugin) $
        plugins'

    -- Instead of sleeping forever, we wait until graceful shutdown
    waitForWorkers allWorkersCount

-- | Run full node in any WorkMode.
runNode
    :: (SscConstraint ssc, WorkMode ssc m)
    => ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode (plugins', plOuts) = (,plOuts <> wOuts) $ runNode' $ workers' ++ plugins''
  where
    (workers', wOuts) = allWorkers
    plugins'' = map (wrapActionSpec "plugin") plugins'

-- | Try to discover peers repeatedly until at least one live peer is found
waitForPeers :: WorkMode ssc m => m ()
waitForPeers = discoverPeers >>= \case
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

initUSMemState :: WorkMode ssc m => m ()
initUSMemState = do
    tip <- GS.getTip
    tvar <- mvState <$> askUSMemVar
    slot <- fromMaybe (SlotId 0 0) <$> getCurrentSlot
    atomically $ writeTVar tvar (MemState slot tip def def)
