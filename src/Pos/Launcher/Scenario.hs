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

import           Control.Lens        (each, to, views, _tail)
import           Development.GitRev  (gitBranch, gitHash)
import           Ether.Internal      (HasLens (..))
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (fork)
import           Paths_cardano_sl    (version)
import           Serokell.Util.Text  (listJson)
import           System.Exit         (ExitCode (..))
import           System.Wlog         (WithLogger, getLoggerName, logError, logInfo,
                                      logWarning)

import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec,
                                      wrapActionSpec)
import qualified Pos.Constants       as Const
import           Pos.Context         (BlkSemaphore (..), HasNodeContext (..),
                                      HasPrimaryKey (..), NodeContext,
                                      getOurPubKeyAddress, getOurPublicKey)
import           Pos.Crypto          (createPsk, encToPublic)
import           Pos.DB              (MonadDB)
import qualified Pos.DB.GState       as GS
import           Pos.DB.Misc         (addProxySecretKey)
import           Pos.Delegation      (initDelegation)
import           Pos.Lrc.DB          as LrcDB
import           Pos.Reporting       (reportMisbehaviourSilent)
import           Pos.Security        (SecurityWorkersClass)
import           Pos.Shutdown        (waitForWorkers)
import           Pos.Slotting        (waitSystemStart)
import           Pos.Ssc.Class       (SscConstraint)
import           Pos.Types           (addressHash)
import           Pos.Util            (inAssertMode)
import           Pos.Util.LogSafe    (logInfoS)
import           Pos.Util.UserSecret (HasUserSecret (..), usKeys)
import           Pos.Worker          (allWorkers, allWorkersCount)
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

    lastKnownEpoch <- LrcDB.getEpoch
    let onNoLeaders = logWarning "Couldn't retrieve last known leaders list"
    let onLeaders leaders =
            logInfo $
            sformat ("Last known leaders for epoch "%build%" are: "%listJson)
                    lastKnownEpoch leaders
    LrcDB.getLeaders lastKnownEpoch >>= maybe onNoLeaders onLeaders

    putProxySecretKeys
    initDelegation @ssc
    initSemaphore
    waitSystemStart
    let unpackPlugin (ActionSpec action) =
            action vI sendActions `catch` reportHandler
    mapM_ (fork . unpackPlugin) plugins'

    nc <- view nodeContext

    -- Instead of sleeping forever, we wait until graceful shutdown
    waitForWorkers (allWorkersCount @ssc nc)
    exitWith (ExitFailure 20)
  where
    -- FIXME shouldn't this kill the whole program?
    reportHandler (SomeException e) = do
        loggerName <- getLoggerName
        reportMisbehaviourSilent version $
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

putProxySecretKeys ::
       ( MonadDB m
       , MonadReader ctx m
       , MonadIO m
       , HasUserSecret ctx
       , HasPrimaryKey ctx )
    => m ()
putProxySecretKeys = do
    uSecret <- atomically . readTVar =<< view userSecret
    secretKey <- view primaryKey
    let eternity = (minBound, maxBound)
        makeOwnPSK =
            flip (createPsk secretKey) eternity . encToPublic
        ownPSKs = uSecret ^.. usKeys . _tail . each . to makeOwnPSK
    for_ ownPSKs addProxySecretKey

initSemaphore :: (WorkMode ssc ctx m) => m ()
initSemaphore = do
    semaphore <- views (lensOf @BlkSemaphore) unBlkSemaphore
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncBlkSemaphore is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip
