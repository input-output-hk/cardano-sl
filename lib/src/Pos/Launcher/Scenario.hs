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
import           Mockable (mapConcurrently)
import           Serokell.Util (listJson)

import           Pos.Context (getOurPublicKey)
import           Pos.Core (GenesisData (gdBootStakeholders, gdHeavyDelegation),
                     GenesisDelegation (..), GenesisWStakeholders (..),
                     addressHash, gdFtsSeed, genesisData)
import           Pos.Crypto (ProtocolMagic, pskDelegatePk)
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (reportOrLogE)
import           Pos.Infra.Slotting (waitSystemStart)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Txp (bootDustThreshold)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     curSoftwareVersion, lastKnownBlockVersion, ourSystemTag)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Util.Trace.Named (TraceNamed, logInfo, logInfoS)
import           Pos.Worker (allWorkers)
import           Pos.WorkMode.Class (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ext ctx m.
       ( HasCompileInfo
       , WorkMode ctx m
       )
    => TraceNamed m
    -> NodeResources ext
    -> [Diffusion m -> m ()]
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode' logTrace NodeResources {..} workers' plugins' = \diffusion -> do
    logInfo logTrace $ "Built with: " <> pretty compileInfo
    nodeStartMsg logTrace
    inAssertMode $ logInfo logTrace "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS logTrace $ sformat ("My public key is: "%build%", pk hash: "%build)
        pk pkHash

    let genesisStakeholders = gdBootStakeholders genesisData
    logInfo logTrace $ sformat
        ("Genesis stakeholders ("%int%" addresses, dust threshold "%build%"): "%build)
        (length $ getGenesisWStakeholders genesisStakeholders)
        bootDustThreshold
        genesisStakeholders

    let genesisDelegation = gdHeavyDelegation genesisData
    let formatDlgPair (issuerId, delegateId) =
            bprint (build%" -> "%build) issuerId delegateId
    logInfo logTrace $ sformat ("GenesisDelegation (stakeholder ids): "%listJson)
            $ map (formatDlgPair . second (addressHash . pskDelegatePk))
            $ HM.toList
            $ unGenesisDelegation genesisDelegation

    firstGenesisHash <- GS.getFirstGenesisBlockHash
    logInfo logTrace $ sformat
        ("First genesis block hash: "%build%", genesis seed is "%build)
        firstGenesisHash
        (gdFtsSeed genesisData)

    tipHeader <- DB.getTipHeader
    logInfo logTrace $ sformat ("Current tip header: "%build) tipHeader

    waitSystemStart logTrace
    let runWithReportHandler action =
            action diffusion `catch` reportHandler

    void (mapConcurrently runWithReportHandler (workers' ++ plugins'))

    exitFailure

  where
    -- FIXME shouldn't this kill the whole program?
    -- FIXME: looks like something bad.
    -- REPORT:ERROR Node's worker/plugin failed with exception (which wasn't caught)
    reportHandler exc@(SomeException e) =
        reportOrLogE logTrace
          (sformat ("Worker/plugin failed with exception: "%shown) e)
          exc

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , HasTxpConfiguration
       , WorkMode ctx m
       )
    => TraceNamed m
    -> ProtocolMagic
    -> NodeResources ext
    -> [Diffusion m -> m ()]
    -> Diffusion m -> m ()
runNode logTrace pm nr plugins = runNode' logTrace nr workers' plugins
  where
    workers' = allWorkers logTrace pm nr

-- | This function prints a very useful message when node is started.
nodeStartMsg :: HasUpdateConfiguration => TraceNamed m -> m ()
nodeStartMsg logTrace = logInfo logTrace msg
  where
    msg = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag
