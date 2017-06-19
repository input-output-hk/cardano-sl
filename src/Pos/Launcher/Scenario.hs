{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import           Control.Lens        (each, to, _tail)
import           Development.GitRev  (gitBranch, gitHash)
import qualified Ether
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (fork)
import           Paths_cardano_sl    (version)
import           System.Exit         (ExitCode (..))
import           System.Wlog         (WithLogger, getLoggerName, logError, logInfo)

import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec,
                                      wrapActionSpec)
import qualified Pos.Constants       as Const
import           Pos.Context         (BlkSemaphore (..), MonadNodeContext, NodeContext,
                                      NodeContextTag, NodeParams (..),
                                      getOurPubKeyAddress, getOurPublicKey)
import           Pos.Crypto          (createProxySecretKey, encToPublic)
import           Pos.DB              (MonadDB)
import qualified Pos.DB.GState       as GS
import           Pos.DB.Misc         (addProxySecretKey)
import           Pos.Delegation      (initDelegation)
import           Pos.Reporting       (reportMisbehaviourMasked)
import           Pos.Security        (SecurityWorkersClass)
import           Pos.Shutdown        (waitForWorkers)
import           Pos.Slotting        (waitSystemStart)
import           Pos.Ssc.Class       (SscConstraint)
import           Pos.Types           (addressHash)
import           Pos.Util            (inAssertMode)
import           Pos.Util.LogSafe    (logInfoS)
import           Pos.Util.UserSecret (usKeys)
import           Pos.Worker          (allWorkers, allWorkersCount)
import           Pos.WorkMode.Class  (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ssc m.
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       , MonadNodeContext ssc m
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
    putProxySecreyKeys
    initDelegation @ssc
    initSemaphore
    waitSystemStart
    let unpackPlugin (ActionSpec action) =
            action vI sendActions `catch` reportHandler
    mapM_ (fork . unpackPlugin) plugins'

    nc <- Ether.ask @NodeContextTag

    -- Instead of sleeping forever, we wait until graceful shutdown
    waitForWorkers (allWorkersCount @ssc nc)
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
runNode ::
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       , MonadNodeContext ssc m
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

putProxySecreyKeys :: (MonadDB m, Ether.MonadReader' NodeParams m) => m ()
putProxySecreyKeys = do
    userSecret <- npUserSecret <$> Ether.ask'
    secretKey <- npSecretKey <$> Ether.ask'
    let eternity = (minBound, maxBound)
        makeOwnPSK =
            flip (createProxySecretKey secretKey) eternity . encToPublic
        ownPSKs = userSecret ^.. usKeys . _tail . each . to makeOwnPSK
    for_ ownPSKs addProxySecretKey

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- Ether.asks' unBlkSemaphore
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncBlkSemaphore is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip
