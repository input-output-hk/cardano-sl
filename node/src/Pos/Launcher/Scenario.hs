{-# LANGUAGE RankNTypes #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import           Control.Lens          (views)
import           Data.Time.Units       (Second)
import           Development.GitRev    (gitBranch, gitHash)
import           Ether.Internal        (HasLens (..))
import           Formatting            (build, int, sformat, shown, (%))
import           Mockable              (fork)
import           Serokell.Util.Text    (listJson)
import           System.Exit           (ExitCode (..))
import           System.Wlog           (WithLogger, getLoggerName, logError, logInfo,
                                        logWarning)

import           Pos.Communication     (ActionSpec (..), OutSpecs, WorkerSpec,
                                        wrapActionSpec)
import qualified Pos.Constants         as Const
import           Pos.Context           (BlkSemaphore (..), getOurPubKeyAddress,
                                        getOurPublicKey, ncNetworkConfig)
import           Pos.DHT.Real          (KademliaDHTInstance (..),
                                        kademliaJoinNetworkNoThrow,
                                        kademliaJoinNetworkRetry)
import           Pos.Genesis           (GenesisWStakeholders (..), bootDustThreshold)
import qualified Pos.GState            as GS
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Lrc.DB            as LrcDB
import           Pos.Network.Types     (NetworkConfig (..), topologyRunKademlia)
import           Pos.Reporting         (reportMisbehaviourSilent)
import           Pos.Security          (SecurityWorkersClass)
import           Pos.Shutdown          (waitForWorkers)
import           Pos.Slotting          (waitSystemStart)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Types             (addressHash)
import           Pos.Util              (inAssertMode)
import           Pos.Util.LogSafe      (logInfoS)
import           Pos.Worker            (allWorkers)
import           Pos.WorkMode.Class    (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ssc ctx m.
       ( WorkMode ssc ctx m
       )
    => NodeResources ssc m
    -> [WorkerSpec m]
    -> [WorkerSpec m]
    -> WorkerSpec m
runNode' NodeResources {..} workers' plugins' = ActionSpec $ \vI sendActions -> do

    logInfo $ "cardano-sl, commit " <> $(gitHash) <> " @ " <> $(gitBranch)
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    addr <- getOurPubKeyAddress
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%
                        ", address: "%build%
                        ", pk hash: "%build) pk addr pkHash

    -- Synchronously join the Kademlia network before doing any more.
    --
    -- See 'topologyRunKademlia' documentation: the second component is 'True'
    -- iff it's essential that at least one of the initial peers is contacted.
    -- Otherwise, it's OK to not find any initial peers and the program can
    -- continue.
    let retryInterval :: Second
        retryInterval = 5
    case topologyRunKademlia (ncTopology (ncNetworkConfig nrContext)) of
        Just (kInst, True)  -> kademliaJoinNetworkRetry kInst (kdiInitialPeers kInst) retryInterval
        Just (kInst, False) -> kademliaJoinNetworkNoThrow kInst (kdiInitialPeers kInst)
        Nothing             -> return ()

    genesisStakeholders <- view (lensOf @GenesisWStakeholders)
    logInfo $ sformat ("Dust threshold: "%build)
        (bootDustThreshold genesisStakeholders)
    logInfo $ sformat ("Genesis stakeholders: " %int)
        (length $ getGenesisWStakeholders genesisStakeholders)

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
    mapM_ (fork . unpackPlugin) workers'
    mapM_ (fork . unpackPlugin) plugins'

    -- Instead of sleeping forever, we wait until graceful shutdown
    -- TBD why don't we also wait for the plugins?
    waitForWorkers (length workers')
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
       )
    => NodeResources ssc m
    -> ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode nr (plugins, plOuts) =
    (, plOuts <> wOuts) $ runNode' nr workers' plugins'
  where
    (workers', wOuts) = allWorkers nr
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
