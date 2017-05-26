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

import           Data.Default       (def)
import           Development.GitRev (gitBranch, gitHash)
import qualified Ether
import           Formatting         (build, sformat, shown, (%))
import           Mockable           (fork)
import           Paths_cardano_sl   (version)
import           System.Exit        (ExitCode (..))
import           System.Wlog        (getLoggerName, logError, logInfo)
import           Universum

import           Pos.Communication  (ActionSpec (..), OutSpecs, WorkerSpec,
                                     wrapActionSpec)
import           Pos.Context        (BlkSemaphore (..), npPubKeyAddress, npPublicKey)
import           Pos.DB.Class       (MonadDBCore)
import qualified Pos.DB.GState      as GS
import           Pos.Delegation     (initDelegation)
import           Pos.Lrc.Context    (LrcSyncData (..), lcLrcSync)
import qualified Pos.Lrc.DB         as LrcDB
import           Pos.Reporting      (reportMisbehaviourMasked)
import           Pos.Shutdown       (waitForWorkers)
import           Pos.Slotting       (getCurrentSlot, waitSystemStart)
import           Pos.Ssc.Class      (SscConstraint)
import           Pos.Types          (SlotId (..), addressHash)
import           Pos.Update         (MemState (..), mvState)
import           Pos.Update.Context (UpdateContext (ucMemState))
import           Pos.Util           (inAssertMode)
import           Pos.Util.LogSafe   (logInfoS)
import           Pos.Worker         (allWorkers, allWorkersCount)
import           Pos.WorkMode.Class (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ssc m.
       (SscConstraint ssc, WorkMode ssc m, MonadDBCore m)
    => [WorkerSpec m]
    -> WorkerSpec m
runNode' plugins' = ActionSpec $ \vI sendActions -> do

    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    inAssertMode $ logInfo "Assert mode on"
    pk <- Ether.asks' npPublicKey
    addr <- Ether.asks' npPubKeyAddress
    let pkHash = addressHash pk

    logInfoS $ sformat ("My public key is: "%build%
                        ", address: "%build%
                        ", pk hash: "%build) pk addr pkHash
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
        reportMisbehaviourMasked version $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: (SscConstraint ssc, WorkMode ssc m, MonadDBCore m)
    => ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode (plugins', plOuts) = (,plOuts <> wOuts) $ runNode' $ workers' ++ plugins''
  where
    (workers', wOuts) = allWorkers
    plugins'' = map (wrapActionSpec "plugin") plugins'

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- Ether.asks' unBlkSemaphore
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncBlkSemaphore is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip

initLrc :: WorkMode ssc m => m ()
initLrc = do
    lrcSync <- Ether.asks' lcLrcSync
    epoch <- LrcDB.getEpoch
    atomically $ writeTVar lrcSync (LrcSyncData True epoch)

initUSMemState :: WorkMode ssc m => m ()
initUSMemState = do
    tip <- GS.getTip
    tvar <- mvState <$> Ether.asks' ucMemState
    slot <- fromMaybe (SlotId 0 minBound) <$> getCurrentSlot
    atomically $ writeTVar tvar (MemState slot tip def def)
