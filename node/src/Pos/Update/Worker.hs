-- | Update System related workers.

module Pos.Update.Worker
       ( usWorkers

       , updateTriggerWorker
       ) where

import           Universum

import           Formatting                 (build, sformat, (%))
import           Serokell.Util.Text         (listJsonIndent)
import           System.Wlog                (logDebug, logInfo)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker,
                                             worker)
import           Pos.Update.Configuration   (curSoftwareVersion)
import           Pos.Context                (recoveryCommGuard)
import           Pos.Core                   (SoftwareVersion (..))
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Update.Context         (UpdateContext (..))
import           Pos.Update.Core            (UpdateProposal (..))
import           Pos.Update.DB              (getConfirmedProposals)
import           Pos.Update.Download        (downloadUpdate)
import           Pos.Update.Logic.Local     (processNewSlot)
import           Pos.Update.Poll            (ConfirmedProposalState (..))
import           Pos.Util.Util              (lensOf)
import           Pos.WorkMode.Class         (WorkMode)

-- | Update System related workers.
usWorkers :: WorkMode ssc ctx m => ([WorkerSpec m], OutSpecs)
usWorkers =
    first pure $
    localOnNewSlotWorker True $ \s ->
        recoveryCommGuard $ do
            logDebug "Updating slot for US..."
            processNewSlot s
            checkForUpdate

checkForUpdate ::
       forall ssc ctx m. WorkMode ssc ctx m
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
updateTriggerWorker :: WorkMode ssc ctx m => ([WorkerSpec m], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar . ucDownloadedUpdate =<< view (lensOf @UpdateContext)
    triggerShutdown
