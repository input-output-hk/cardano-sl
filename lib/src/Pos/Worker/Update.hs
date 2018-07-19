-- | Update System related workers.

module Pos.Worker.Update
       ( usWorkers

       , updateTriggerWorker
       ) where

import           Universum

import           Formatting (build, sformat, (%))
import           Serokell.Util.Text (listJsonIndent)
import           System.Wlog (logDebug, logInfo)

import           Pos.Core (SoftwareVersion (..))
import           Pos.Core.Update (UpdateProposal (..))
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Recovery.Info (recoveryCommGuard)
import           Pos.Infra.Shutdown (triggerShutdown)
import           Pos.Infra.Slotting.Util (ActionTerminationPolicy (..),
                     OnNewSlotParams (..), defaultOnNewSlotParams, onNewSlot)
import           Pos.Listener.Update (UpdateMode)
import           Pos.Network.Update.Download (downloadUpdate)
import           Pos.Update.Configuration (curSoftwareVersion)
import           Pos.Update.Context (UpdateContext (..))
import           Pos.Update.DB (getConfirmedProposals)
import           Pos.Update.Logic.Local (processNewSlot)
import           Pos.Update.Poll.Types (ConfirmedProposalState (..))
import           Pos.Util.Util (lensOf)

-- | Update System related workers.
usWorkers
    :: forall ctx m.
       ( UpdateMode ctx m
       )
    => [Diffusion m -> m ()]
usWorkers = [processNewSlotWorker, checkForUpdateWorker]
  where
    -- These are two separate workers. We want them to run in parallel
    -- and not affect each other.
    processNewSlotParams = defaultOnNewSlotParams
        { onspTerminationPolicy =
              NewSlotTerminationPolicy "Update.processNewSlot"
        }
    processNewSlotWorker = \_ ->
        onNewSlot processNewSlotParams $ \s ->
            recoveryCommGuard "processNewSlot in US" $ do
                logDebug "Updating slot for US..."
                processNewSlot s
    checkForUpdateWorker = \_ ->
        onNewSlot defaultOnNewSlotParams $ \_ ->
            recoveryCommGuard "checkForUpdate" (checkForUpdate @ctx @m)

checkForUpdate ::
       forall ctx m. UpdateMode ctx m
    => m ()
checkForUpdate = do
    logDebug "Checking for update..."
    confirmedProposals <-
        getConfirmedProposals (Just $ svNumber curSoftwareVersion)
    case nonEmpty confirmedProposals of
        Nothing ->
            logDebug
                "There are no new confirmed update proposals for our application"
        Just confirmedProposalsNE -> processProposals confirmedProposalsNE
  where
    processProposals :: NonEmpty ConfirmedProposalState -> m ()
    processProposals confirmedProposals = do
        let cpsToNumericVersion =
                svNumber . upSoftwareVersion . cpsUpdateProposal
        let newestCPS =
                maximumBy (comparing cpsToNumericVersion) confirmedProposals
        logInfo $
            sformat
                ("There are new confirmed update proposals for our application: "
                 %listJsonIndent 2%
                 "\n The newest one is: "%build%" and we want to download it")
                (cpsUpdateProposal <$> confirmedProposals)
                (cpsUpdateProposal newestCPS)
        downloadUpdate newestCPS

-- | This worker is just waiting until we download an update for our
-- application. When an update is downloaded, it shuts the system
-- down. It should be used in there is no high-level code which shuts
-- down the system (e. g. in regular node w/o wallet or in explorer).
updateTriggerWorker :: UpdateMode ctx m => Diffusion m -> m ()
updateTriggerWorker = \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar . ucDownloadedUpdate =<< view (lensOf @UpdateContext)
    triggerShutdown
