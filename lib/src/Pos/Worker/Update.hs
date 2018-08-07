-- | Update System related workers.

module Pos.Worker.Update
       ( usWorkers

       , updateTriggerWorker
       ) where

import           Universum

import           Formatting (build, sformat, (%))
import           Serokell.Util.Text (listJsonIndent)

import           Pos.Chain.Update (ConfirmedProposalState (..),
                     curSoftwareVersion)
import           Pos.Core.Update (SoftwareVersion (..), UpdateProposal (..))
import           Pos.DB.Update (UpdateContext (..), getConfirmedProposals,
                     processNewSlot)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Recovery.Info (recoveryCommGuard)
import           Pos.Infra.Shutdown (triggerShutdown)
import           Pos.Infra.Slotting.Util (ActionTerminationPolicy (..),
                     OnNewSlotParams (..), defaultOnNewSlotParams, onNewSlot)
import           Pos.Listener.Update (UpdateMode)
import           Pos.Network.Update.Download (downloadUpdate)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logInfo)
import           Pos.Util.Util (lensOf)

-- | Update System related workers.
usWorkers
    :: forall ctx m.
       ( UpdateMode ctx m
       )
    => TraceNamed m
    -> [Diffusion m -> m ()]
usWorkers logTrace = [processNewSlotWorker, checkForUpdateWorker]
  where
    -- These are two separate workers. We want them to run in parallel
    -- and not affect each other.
    processNewSlotParams = defaultOnNewSlotParams
        { onspTerminationPolicy =
              NewSlotTerminationPolicy "Update.processNewSlot"
        }
    processNewSlotWorker = \_ ->
        onNewSlot logTrace processNewSlotParams $ \s ->
            recoveryCommGuard logTrace "processNewSlot in US" $ do
                logDebug logTrace "Updating slot for US..."
                processNewSlot logTrace s
    checkForUpdateWorker = \_ ->
        onNewSlot logTrace defaultOnNewSlotParams $ \_ ->
            recoveryCommGuard logTrace "checkForUpdate" (checkForUpdate @ctx @m logTrace)

checkForUpdate
    :: forall ctx m.
       ( UpdateMode ctx m
       )
    => TraceNamed m
    -> m ()
checkForUpdate logTrace = do
    logDebug logTrace "Checking for update..."
    confirmedProposals <-
        getConfirmedProposals (Just $ svNumber curSoftwareVersion)
    case nonEmpty confirmedProposals of
        Nothing ->
            logDebug logTrace
                "There are no new confirmed update proposals for our application"
        Just confirmedProposalsNE -> processProposals confirmedProposalsNE
  where
    processProposals :: NonEmpty ConfirmedProposalState -> m ()
    processProposals confirmedProposals = do
        let cpsToNumericVersion =
                svNumber . upSoftwareVersion . cpsUpdateProposal
        let newestCPS =
                maximumBy (comparing cpsToNumericVersion) confirmedProposals
        logInfo logTrace $
            sformat
                ("There are new confirmed update proposals for our application: "
                 %listJsonIndent 2%
                 "\n The newest one is: "%build%" and we want to download it")
                (cpsUpdateProposal <$> confirmedProposals)
                (cpsUpdateProposal newestCPS)
        downloadUpdate logTrace newestCPS

-- | This worker is just waiting until we download an update for our
-- application. When an update is downloaded, it shuts the system
-- down. It should be used in there is no high-level code which shuts
-- down the system (e. g. in regular node w/o wallet or in explorer).
updateTriggerWorker
    :: UpdateMode ctx m
    => TraceNamed m
    -> Diffusion m -> m ()
updateTriggerWorker logTrace = \_ -> do
    logInfo logTrace "Update trigger worker is locked"
    void $ takeMVar . ucDownloadedUpdate =<< view (lensOf @UpdateContext)
    triggerShutdown logTrace
