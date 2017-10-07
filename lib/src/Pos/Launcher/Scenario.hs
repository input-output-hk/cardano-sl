{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import           Data.Time.Units          (Second)
import           Formatting               (build, int, sformat, shown, (%))
import           Mockable                 (mapConcurrently, race)
import           Serokell.Util.Text       (listJson)
import           System.Exit              (ExitCode (..))
import           System.Wlog              (WithLogger, getLoggerName, logInfo, logWarning)

import           Pos.Communication        (ActionSpec (..), OutSpecs, WorkerSpec,
                                           wrapActionSpec)
import           Pos.Context              (getOurPublicKey, ncNetworkConfig)
import           Pos.Core                 (GenesisData (gdBootStakeholders),
                                           GenesisWStakeholders (..), addressHash,
                                           gdFtsSeed, genesisData)
import qualified Pos.DB.DB                as DB
import           Pos.DHT.Real             (KademliaDHTInstance (..),
                                           kademliaJoinNetworkNoThrow,
                                           kademliaJoinNetworkRetry)
import qualified Pos.GState               as GS
import           Pos.Launcher.Resource    (NodeResources (..))
import           Pos.Lrc.DB               as LrcDB
import           Pos.Network.Types        (NetworkConfig (..), topologyRunKademlia)
import           Pos.Reporting            (reportError)
import           Pos.Shutdown             (waitForShutdown)
import           Pos.Slotting             (waitSystemStart)
import           Pos.Ssc.Class            (SscConstraint)
import           Pos.Txp                  (bootDustThreshold)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion,
                                           lastKnownBlockVersion, ourSystemTag)
import           Pos.Util                 (inAssertMode)
import           Pos.Util.CompileInfo     (compileInfo)
import           Pos.Util.LogSafe         (logInfoS)
import           Pos.Worker               (allWorkers)
import           Pos.WorkMode.Class       (WorkMode)

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
    logInfo $ "Built with: " <> pretty compileInfo
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
        bootDustThreshold
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
            sformat ("Last known leaders for epoch "%build%" are: "%listJson)
                    lastKnownEpoch leaders
    LrcDB.getLeadersForEpoch lastKnownEpoch >>= maybe onNoLeaders onLeaders
    tipHeader <- DB.getTipHeader @ssc
    logInfo $ sformat ("Current tip header: "%build) tipHeader

    waitSystemStart
    let unpackPlugin (ActionSpec action) =
            action vI sendActions `catch` reportHandler

    -- Either all the plugins are cancelled in the case one of them
    -- throws an error, or otherwise when the shutdown signal comes,
    -- they are killed automatically.
    void
      (race
           (void (mapConcurrently (unpackPlugin) $ workers' ++ plugins'))
           waitForShutdown)

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
    msg = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag

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
