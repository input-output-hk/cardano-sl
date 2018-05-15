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
import           Formatting (bprint, build, int, sformat, shown, (%))
import           Mockable (Mockable, Async, mapConcurrently)
import           Serokell.Util (listJson)
import           System.Wlog (WithLogger, askLoggerName, logInfo)

import           Pos.Context (getOurPublicKey)
import           Pos.Core (GenesisData (gdBootStakeholders, gdHeavyDelegation),
                           GenesisDelegation (..), GenesisWStakeholders (..), addressHash,
                           gdFtsSeed, genesisData)
import           Pos.Crypto (pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.GState as GS
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Reporting (reportError)
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
       ( HasCompileInfo
       , WorkMode ctx m
       , Mockable Async m
       )
    => NodeResources ext
    -> [Diffusion m -> m ()]
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode' NodeResources {..} workers' plugins' = \diffusion -> do
    logInfo $ "Built with: " <> pretty compileInfo
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%", pk hash: "%build)
        pk pkHash

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
    let runWithReportHandler action =
            action diffusion `catch` reportHandler

    void (mapConcurrently runWithReportHandler (workers' ++ plugins'))

    exitFailure

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

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , WorkMode ctx m
       )
    => NodeResources ext
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode nr plugins = runNode' nr workers' plugins
  where
    workers' = allWorkers nr

-- | This function prints a very useful message when node is started.
nodeStartMsg :: (HasUpdateConfiguration, WithLogger m) => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag
