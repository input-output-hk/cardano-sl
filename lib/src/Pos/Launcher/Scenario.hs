{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (bprint, build, int, sformat, shown, (%))
import           Serokell.Util (listJson)

import           Pos.Chain.Genesis as Genesis (Config (..),
                     GenesisDelegation (..), GenesisWStakeholders (..),
                     configBootStakeholders, configFtsSeed,
                     configHeavyDelegation)
import           Pos.Chain.Txp (TxpConfiguration, bootDustThreshold)
import           Pos.Chain.Update (UpdateConfiguration, curSoftwareVersion,
                     lastKnownBlockVersion, ourSystemTag)
import           Pos.Context (NodeContext (..), getOurPublicKey, npSecretKey)
import           Pos.Core (addressHash)
import           Pos.Core.Conc (mapConcurrently)
import           Pos.Crypto (pskDelegatePk, toPublic)
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.GState as GS
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Reporting (reportError)
import           Pos.Infra.Slotting (waitSystemStart)
import           Pos.Infra.Util.LogSafe (logInfoS)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Util.Util (HasLens', lensOf)
import           Pos.Util.Wlog (WithLogger, askLoggerName, logInfo)
import           Pos.Worker (allWorkers)
import           Pos.WorkMode.Class (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ext ctx m.
       ( HasCompileInfo
       , WorkMode ctx m
       )
    => Genesis.Config
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
    -> [ (Text, Diffusion m -> m ()) ]
    -> Diffusion m -> m ()
runNode' genesisConfig NodeResources {..} workers' plugins' = \diffusion -> do
    logInfo $ "Built with: " <> pretty compileInfo
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%", pk hash: "%build)
        pk pkHash

    let genesisStakeholders = configBootStakeholders genesisConfig
    logInfo $ sformat
        ("Genesis stakeholders ("%int%" addresses, dust threshold "%build%"): "%build)
        (length $ getGenesisWStakeholders genesisStakeholders)
        (bootDustThreshold genesisStakeholders)
        genesisStakeholders

    let genesisDelegation = configHeavyDelegation genesisConfig
    let formatDlgPair (issuerId, delegateId) =
            bprint (build%" -> "%build) issuerId delegateId
    logInfo $ sformat ("GenesisDelegation (stakeholder ids): "%listJson)
            $ map (formatDlgPair . second (addressHash . pskDelegatePk))
            $ HM.toList
            $ unGenesisDelegation genesisDelegation

    firstGenesisHash <- GS.getFirstGenesisBlockHash $ configGenesisHash
        genesisConfig
    logInfo $ sformat
        ("First genesis block hash: "%build%", genesis seed is "%build)
        firstGenesisHash
        (configFtsSeed genesisConfig)

    tipHeader <- DB.getTipHeader
    logInfo $ sformat ("Current tip header: "%build) tipHeader

    waitSystemStart
    let
      runWithReportHandler :: (Text, Diffusion m -> m ()) -> m ()
      runWithReportHandler (workerName, action) = action diffusion `catch` (reportHandler workerName)

    void (mapConcurrently runWithReportHandler (workers' ++ plugins'))

    exitFailure
  where
    reportHandler :: Text -> SomeException -> m b
    reportHandler action (SomeException e) = do
        loggerName <- askLoggerName
        let msg = "Worker/plugin with work name "%shown%" and logger name "%shown%" failed with exception: "%shown
        reportError $ sformat msg action loggerName e
        exitFailure

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , WorkMode ctx m
       )
    => Genesis.Config
    -> TxpConfiguration
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
    -> Diffusion m -> m ()
runNode genesisConfig txpConfig nr plugins =
    runNode' genesisConfig nr workers' plugins
    where workers' = allWorkers sid genesisConfig txpConfig nr
          sid = addressHash . toPublic . npSecretKey . ncNodeParams . nrContext
              $ nr

-- | This function prints a very useful message when node is started.
nodeStartMsg
    :: (MonadReader r m, HasLens' r UpdateConfiguration, WithLogger m)
    => m ()
nodeStartMsg = do
    uc <- view (lensOf @UpdateConfiguration)
    logInfo (msg uc)
  where
    msg uc = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   (curSoftwareVersion uc)
                   (lastKnownBlockVersion uc)
                   (ourSystemTag uc)
