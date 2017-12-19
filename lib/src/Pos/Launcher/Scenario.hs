{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Second)
import           Formatting (bprint, build, int, sformat, shown, (%))
import           Mockable (mapConcurrently, race)
import           Serokell.Util (listJson)
import           System.Exit (ExitCode (..))
import           System.Wlog (WithLogger, askLoggerName, logDebug, logInfo, logWarning)

import           Pos.Communication (ActionSpec (..), OutSpecs, WorkerSpec, wrapActionSpec)
import           Pos.Context (getOurPublicKey, ncNetworkConfig)
import           Pos.Core (GenesisData (gdBootStakeholders, gdHeavyDelegation),
                           GenesisDelegation (..), GenesisWStakeholders (..), addressHash,
                           gdFtsSeed, genesisData)
import           Pos.Crypto (pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DHT.Real (KademliaDHTInstance (..), kademliaJoinNetworkNoThrow,
                               kademliaJoinNetworkRetry)
import qualified Pos.GState as GS
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Network.Types (NetworkConfig (..), topologyRunKademlia)
import           Pos.NtpCheck (NtpStatus (..), ntpSettings, withNtpCheck)
import           Pos.Reporting (reportError)
import           Pos.Shutdown (waitForShutdown)
import           Pos.Slotting (waitSystemStart)
import           Pos.Txp (bootDustThreshold)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion,
                                           lastKnownBlockVersion, ourSystemTag)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Util.LogSafe (logInfoS)
import           Pos.Worker (allWorkers)
import           Pos.WorkMode.Class (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ext ctx m.
       ( HasCompileInfo, WorkMode ctx m
       )
    => NodeResources ext m
    -> [WorkerSpec m]
    -> [WorkerSpec m]
    -> WorkerSpec m
runNode' NodeResources {..} workers' plugins' = ActionSpec $ \vI sendActions -> ntpCheck $ do
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

    let genesisDelegation = gdHeavyDelegation genesisData
    let formatDlgPair (issuerId, delegateId) =
            bprint (build%" -> "%build) issuerId delegateId
    logInfo $ sformat ("GenesisDelegation (stakeholder ids): "%listJson)
            $ map (formatDlgPair . second (addressHash . pskDelegatePk))
            $ HM.toList
            $ unGenesisDelegation genesisDelegation

    firstGenesisHash <- GS.getFirstGenesisBlockHash
    logInfo $ sformat
        ("First genesis block hash: "%build%", genesis seed is "%build)
        firstGenesisHash
        (gdFtsSeed genesisData)

    tipHeader <- DB.getTipHeader
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
        loggerName <- askLoggerName
        reportError $
            sformat ("Worker/plugin with logger name "%shown%
                    " failed with exception: "%shown)
            loggerName e
    ntpCheck = withNtpCheck $ ntpSettings onNtpStatusLogWarning

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , WorkMode ctx m
       )
    => NodeResources ext m
    -> ([WorkerSpec m], OutSpecs)
    -> (WorkerSpec m, OutSpecs)
runNode nr (plugins, plOuts) =
    (, plOuts <> wOuts) $ runNode' nr workers' plugins'
  where
    (workers', wOuts) = allWorkers nr
    plugins' = map (wrapActionSpec "plugin") plugins

onNtpStatusLogWarning :: WithLogger m => NtpStatus -> m ()
onNtpStatusLogWarning = \case
    NtpSyncOk -> logDebug $
              -- putText  $ -- FIXME: for some reason this message isn't printed
                            -- when using 'logDebug', but a simple 'putText' works
                            -- just fine.
        "Local time is in sync with the NTP server"
    NtpDesync diff -> logWarning $
        "Local time is severely off sync with the NTP server: " <> show diff

-- | This function prints a very useful message when node is started.
nodeStartMsg :: (HasUpdateConfiguration, WithLogger m) => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag
