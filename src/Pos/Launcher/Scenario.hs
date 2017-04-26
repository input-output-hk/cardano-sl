{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , initLrc
       , runNode'
       ) where

import           Data.Default           (def)
import           Development.GitRev     (gitBranch, gitHash)
import           Formatting             (build, sformat, shown, (%))
import           Mockable               (fork)
import           Paths_cardano_sl       (version)
import           Serokell.Util          (sec)
import           System.Exit            (ExitCode (..))
import           System.Wlog            (getLoggerName, logError, logInfo)
import           Universum

import           Pos.Communication      (ActionSpec (..), NodeId, OutSpecs, WorkerSpec,
                                         wrapActionSpec)
import           Pos.Context            (NodeContext (..), getNodeContext,
                                         ncPubKeyAddress, ncPublicKey)
import           Pos.DB.Class           (MonadDBCore)
import qualified Pos.DB.GState          as GS
import           Pos.Delegation         (initDelegation)
import           Pos.Launcher.Resources (RealModeResources (..))
import           Pos.Lrc.Context        (LrcSyncData (..), lcLrcSync)
import qualified Pos.Lrc.DB             as LrcDB
import           Pos.Reporting          (reportMisbehaviourMasked)
import           Pos.Shutdown           (waitForWorkers)
import           Pos.Slotting           (getCurrentSlot, waitSystemStart)
import           Pos.Ssc.Class          (SscConstraint)
import           Pos.Types              (SlotId (..), addressHash)
import           Pos.Update             (MemState (..), mvState)
import           Pos.Update.Context     (UpdateContext (ucMemState))
import           Pos.Util               (inAssertMode, waitRandomInterval)
import           Pos.Util.Context       (askContext)
import           Pos.Util.LogSafe       (logInfoS)
import           Pos.Worker             (allWorkers, allWorkersCount)
import           Pos.WorkMode           (WorkMode)

-- | Run full node in any WorkMode.
runNode'
    :: forall ssc m.
       (SscConstraint ssc, WorkMode ssc m, MonadDBCore m)
    => RealModeResources m
    -> [WorkerSpec m]
    -> WorkerSpec m
runNode' res plugins' = ActionSpec $ \vI sendActions -> do

    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    addr <- ncPubKeyAddress <$> getNodeContext
    let pkHash = addressHash pk

    logInfoS $ sformat ("My public key is: "%build%
                        ", address: "%build%
                        ", pk hash: "%build) pk addr pkHash
    () <$ fork (waitForPeers (rmFindPeers res))
    initDelegation @ssc
    initLrc
    initUSMemState
    initSemaphore
    waitSystemStart
    let unpackPlugin (ActionSpec action) =
            action vI sendActions `catch` reportHandler
    mapM_ (fork . unpackPlugin) plugins'

    -- Instead of sleeping forever, we wait until graceful shutdown
    waitForWorkers (allWorkersCount @ssc @m)
    exitWith (ExitFailure 20)
  where
    -- FIXME shouldn't this kill the whole program?
    reportHandler (SomeException e) = do
        loggerName <- getLoggerName
        reportMisbehaviourMasked (rmGetPeers res) version $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e

-- | Run full node in any WorkMode.
runNode
    :: (SscConstraint ssc, WorkMode ssc m, MonadDBCore m)
    => RealModeResources m
    -> ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode res (plugins', plOuts) = (,plOuts <> wOuts) $ runNode' res $ workers' ++ plugins''
  where
    (workers', wOuts) = allWorkers (rmGetPeers res)
    plugins'' = map (wrapActionSpec "plugin") plugins'

-- | Try to discover peers repeatedly until at least one live peer is found
--
-- FIXME seems an interrupt-style system would be better. The discovery
-- system can call you back when a new node comes in.
-- Would that be a good model? Run some IO for every peer?
waitForPeers :: WorkMode ssc m => m (Set NodeId) -> m ()
waitForPeers discoverPeers = discoverPeers >>= \s -> case toList s of
    ps@(_:_) -> () <$ logInfo (sformat ("Known peers: "%shown) ps)
    []       -> logInfo "Couldn't connect to any peer, trying again..." >>
                waitRandomInterval (sec 3) (sec 10) >>
                waitForPeers discoverPeers

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- ncBlkSemaphore <$> getNodeContext
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncBlkSemaphore is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip

initLrc :: WorkMode ssc m => m ()
initLrc = do
    lrcSync <- askContext lcLrcSync
    epoch <- LrcDB.getEpoch
    atomically $ writeTVar lrcSync (LrcSyncData True epoch)

initUSMemState :: WorkMode ssc m => m ()
initUSMemState = do
    tip <- GS.getTip
    tvar <- mvState <$> askContext @UpdateContext ucMemState
    slot <- fromMaybe (SlotId 0 0) <$> getCurrentSlot
    atomically $ writeTVar tvar (MemState slot tip def def)
