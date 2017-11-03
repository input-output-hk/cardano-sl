{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , runNode'
       , nodeStartMsg
       , gitRev
       ) where

import           Universum

import           Control.Lens             (views)
import           Data.Time.Units          (Second)
import           Ether.Internal           (HasLens (..))
import           Formatting               (build, int, sformat, shown, (%))
import           Mockable                 (fork)
import           System.Exit              (ExitCode (..))
import           System.Wlog              (WithLogger, getLoggerName, logError, logInfo,
                                           logWarning)

import           Pos.Communication        (ActionSpec (..), OutSpecs, WorkerSpec,
                                           wrapActionSpec)
import           Pos.Context              (getOurPublicKey, ncNetworkConfig)
import           Pos.Core                 (GenesisData (gdBootStakeholders),
                                           GenesisWStakeholders (..), addressHash,
                                           bootDustThreshold, gdFtsSeed, genesisData)
import qualified Pos.DB.DB                as DB
import           Pos.DHT.Real             (KademliaDHTInstance (..),
                                           kademliaJoinNetworkNoThrow,
                                           kademliaJoinNetworkRetry)
import qualified Pos.GState               as GS
import           Pos.Launcher.Resource    (NodeResources (..))
import           Pos.Lrc.DB               as LrcDB
import           Pos.Network.Types        (NetworkConfig (..), topologyRunKademlia)
import           Pos.Reporting            (reportError)
import           Pos.Shutdown             (waitForWorkers)
import           Pos.Slotting             (waitSystemStart)
import           Pos.Ssc.Class            (SscConstraint)
import           Pos.StateLock            (StateLock (..))
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion,
                                           lastKnownBlockVersion)
import           Pos.Util                 (inAssertMode, listChunkedJson)
import           Pos.Util.LogSafe         (logInfoS)
import           Pos.Worker               (allWorkers)
import           Pos.WorkMode.Class       (WorkMode)


#define QUOTED(x) "/**/x/**/"

gitRev :: Text
#if !defined(GITREV)
gitRev = "unknown"
#else
gitRev = QUOTED(GITREV)
#endif

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

    logInfo $ "cardano-sl: commit " <> gitRev
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%", pk hash: "%build)
        pk pkHash

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

    let genesisStakeholders = gdBootStakeholders genesisData
    logInfo $ sformat
        ("Genesis stakeholders ("%int%" addresses, dust threshold "%build%"): "%build)
        (length $ getGenesisWStakeholders genesisStakeholders)
        (bootDustThreshold genesisStakeholders)
        genesisStakeholders
    firstGenesisHash <- GS.getFirstGenesisBlockHash
    logInfo $ sformat
        ("First genesis block hash: "%build%", genesis seed is "%build)
        firstGenesisHash
        (gdFtsSeed genesisData)

    lastKnownEpoch <- LrcDB.getEpoch
    let onNoLeaders = logWarning "Couldn't retrieve last known leaders list"
    let onLeaders leaders =
            logInfo $
            sformat ("Last known leaders for epoch "%build%" are: "%listChunkedJson 5)
                    lastKnownEpoch leaders
    LrcDB.getLeadersForEpoch lastKnownEpoch >>= maybe onNoLeaders onLeaders
    tipHeader <- DB.getTipHeader @ssc
    logInfo $ sformat ("Current tip header: "%build) tipHeader

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
    -- REPORT:ERROR Node's worker/plugin failed with exception (which wasn't caught)
    reportHandler (SomeException e) = do
        loggerName <- getLoggerName
        reportError $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode ::
       ( SscConstraint ssc
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
nodeStartMsg :: (HasUpdateConfiguration, WithLogger m) => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %build% ", last known block version " %build)
                   curSoftwareVersion lastKnownBlockVersion

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
    semaphore <- views (lensOf @StateLock) slTip
    whenJustM (tryReadMVar semaphore) $ const $
        logError "ncStateLock is not empty at the very beginning"
    tip <- GS.getTip
    putMVar semaphore tip
