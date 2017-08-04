{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import           Control.Lens        (views)
import           Development.GitRev  (gitBranch, gitHash)
import           Ether.Internal      (HasLens (..))
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (race, mapConcurrently)
import           Serokell.Util.Text  (listJson)
import           System.Exit         (ExitCode (..))
import           System.Wlog         (WithLogger, getLoggerName, logError, logInfo,
                                      logWarning)

import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec,
                                      wrapActionSpec)
import qualified Pos.Constants       as Const
import           Pos.Context         (BlkSemaphore (..), HasNodeContext (..), NodeContext,
                                      genesisStakeholdersM, getOurPubKeyAddress,
                                      getOurPublicKey)
import qualified Pos.GState          as GS
import           Pos.Shutdown.Logic (waitForShutdown)
import           Pos.Lrc.DB          as LrcDB
import           Pos.Reporting       (reportMisbehaviourSilent)
import           Pos.Security        (SecurityWorkersClass)
import           Pos.Slotting        (waitSystemStart)
import           Pos.Ssc.Class       (SscConstraint)
import           Pos.Types           (addressHash)
import           Pos.Util            (inAssertMode)
import           Pos.Util.LogSafe    (logInfoS)
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Worker          (allWorkers)
import           Pos.WorkMode.Class  (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ssc ctx m.
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc ctx m
       , HasNodeContext ssc ctx
       , HasUserSecret ctx
       )
    => [WorkerSpec m]
    -> WorkerSpec m
runNode' plugins' = ActionSpec $ \vI sendActions -> do

    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    addr <- getOurPubKeyAddress
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%
                        ", address: "%build%
                        ", pk hash: "%build) pk addr pkHash

    genesisStakeholders <- genesisStakeholdersM
    logInfo $ sformat ("Genesis stakeholders: " %listJson) genesisStakeholders

    lastKnownEpoch <- LrcDB.getEpoch
    let onNoLeaders = logWarning "Couldn't retrieve last known leaders list"
    let onLeaders leaders =
            logInfo $
            sformat ("Last known leaders for epoch "%build%" are: "%listJson)
                    lastKnownEpoch leaders
    LrcDB.getLeaders lastKnownEpoch >>= maybe onNoLeaders onLeaders

    initSemaphore
    waitSystemStart
    let unpackPlugin (ActionSpec action) =
            action vI sendActions `catch` reportHandler

    -- Either all the plugins are cancelled in the case one of them
    -- throws an error, or otherwise when the shutdown signal comes,
    -- they are killed automatically.
    void
      (race
           (void (mapConcurrently (unpackPlugin) plugins'))
           waitForShutdown)
    exitWith (ExitFailure 20)
  where
    -- FIXME shouldn't this kill the whole program?
    -- FIXME: looks like something bad.
    -- FIXME [CSL-1340]: it should be reported as 'RError'.
    reportHandler (SomeException e) = do
        loggerName <- getLoggerName
        reportMisbehaviourSilent False $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode ::
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc ctx m
       , HasNodeContext ssc ctx
       , HasUserSecret ctx
       )
    => NodeContext ssc
    -> ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode nc (plugins, plOuts) =
    (, plOuts <> wOuts) $ runNode' $ workers' ++ plugins'
  where
    (workers', wOuts) = allWorkers nc
    plugins' = map (wrapActionSpec "plugin") plugins

-- | This function prints a very useful message when node is started.
nodeStartMsg :: WithLogger m => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %build% ", last known block version " %build)
                   Const.curSoftwareVersion Const.lastKnownBlockVersion

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

-- TODO @pva701: somebody who knows what is going on here fix it.
-- We delegate the right to produce block to node @encToPublic encryptedSK@,
-- why does such node exist?
-- If this function is correct:
-- 1. explain me why it's correct.
-- 2. please don't run it in dev mode, because of there is not a node with delegated PK
-- and node2 doesn't produce blocks and the error about poor chain quality appears in the log.

-- putProxySecretKeys ::
--        ( MonadDB m
--        , MonadReader ctx m
--        , MonadIO m
--        , HasUserSecret ctx
--        , HasPrimaryKey ctx )
--     => m ()
-- putProxySecretKeys = do
--     uSecret <- atomically . readTVar =<< view userSecret
--     secretKey <- view primaryKey
--     let eternity = (minBound, maxBound)
--         makeOwnPSK =
--             flip (createPsk secretKey) eternity . encToPublic
--         ownPSKs = uSecret ^.. usKeys . _tail . each . to makeOwnPSK
--     for_ ownPSKs addProxySecretKey

initSemaphore :: (WorkMode ssc ctx m) => m ()
initSemaphore = do
    semaphore <- views (lensOf @BlkSemaphore) unBlkSemaphore
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncBlkSemaphore is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip
