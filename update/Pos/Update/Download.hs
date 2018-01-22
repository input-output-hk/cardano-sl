{-# LANGUAGE RankNTypes #-}

-- | Logic related to downloading update.

module Pos.Update.Download
       ( installerHash
       , downloadUpdate
       ) where

import           Universum

import           Control.Exception.Safe (handleAny)
import           Control.Lens (views)
import           Control.Monad.Except (ExceptT (..), throwError)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Ether.Internal (HasLens (..))
import           Formatting (build, sformat, stext, (%))
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple (getResponseBody, getResponseStatus, getResponseStatusCode,
                                      httpLBS, parseRequest, setRequestManager)
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Text (listJsonIndent, mapJson)
import           System.Directory (doesFileExist)
import           System.Wlog (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Binary.Class (Raw)
import           Pos.Binary.Update ()
import           Pos.Core.Update (SoftwareVersion (..), UpdateData (..),
                                  UpdateProposal (..))
import           Pos.Crypto (Hash, castHash, hash)
import           Pos.Exception (reportFatalError)
import           Pos.Reporting (reportOrLogW)
import           Pos.Update.Configuration (curSoftwareVersion, ourSystemTag)
import           Pos.Update.Context (UpdateContext (..))
import           Pos.Update.DB.Misc (isUpdateInstalled)
import           Pos.Update.Mode (UpdateMode)
import           Pos.Update.Params (UpdateParams (..))
import           Pos.Update.Poll.Types (ConfirmedProposalState (..))
import           Pos.Util.Concurrent (withMVar)
import           Pos.Util.Util ((<//>))

-- | Compute hash of installer, this is hash is 'udPkgHash' from 'UpdateData'.
--
-- NB: we compute it by first CBOR-encoding it and then applying hash
-- function, which is a bit strange, but it's done for historical
-- reasons.
installerHash :: LByteString -> Hash Raw
installerHash = castHash . hash

-- | Download a software update for given 'ConfirmedProposalState' and
-- put it into a variable which holds 'ConfirmedProposalState' of
-- downloaded update.
-- Parallel downloads aren't supported, so this function may blocks.
-- If we have already downloaded an update successfully, this function won't
-- download new updates.
--
-- The caller must ensure that:
-- 1. This update is for our software.
-- 2. This update is for our system (according to system tag).
-- 3. This update brings newer software version than our current version.
downloadUpdate :: forall ctx m . UpdateMode ctx m => ConfirmedProposalState -> m ()
downloadUpdate cps = do
    downloadLock <- ucDownloadLock <$> view (lensOf @UpdateContext)
    withMVar downloadLock $ \() -> do
        downloadedUpdateMVar <-
            ucDownloadedUpdate <$> view (lensOf @UpdateContext)
        tryReadMVar downloadedUpdateMVar >>= \case
            Nothing -> do
                updHash <- getUpdateHash cps
                installed <- isUpdateInstalled updHash
                if installed
                    then onAlreadyInstalled
                    else downloadUpdateDo updHash cps
            Just existingCPS -> onAlreadyDownloaded existingCPS
  where
    proposalToDL = cpsUpdateProposal cps
    onAlreadyDownloaded ConfirmedProposalState {..} =
        logInfo $
        sformat
            ("We won't download an update for proposal "%build%
             ", because we have already downloaded another update: "%build%
             " and we are waiting for it to be applied")
            proposalToDL
            cpsUpdateProposal
    onAlreadyInstalled =
        logInfo $
        sformat
            ("We won't download an update for proposal "%build%
             ", because it's already installed")
            proposalToDL

getUpdateHash :: UpdateMode ctx m => ConfirmedProposalState -> m (Hash Raw)
getUpdateHash ConfirmedProposalState{..} = do
    useInstaller <- views (lensOf @UpdateParams) upUpdateWithPkg

    let data_ = upData cpsUpdateProposal
        dataHash = if useInstaller then udPkgHash else udAppDiffHash
        mupdHash = dataHash <$> HM.lookup ourSystemTag data_

    logDebug $ sformat ("Proposal's upData: "%mapJson) data_

    -- It must be enforced by the caller.
    maybe (reportFatalError $ sformat
            ("We are trying to download an update not for our "%
            "system, update proposal is: "%build)
            cpsUpdateProposal)
        pure
        mupdHash

-- Download and save archive update by given `ConfirmedProposalState`
downloadUpdateDo :: UpdateMode ctx m => Hash Raw -> ConfirmedProposalState -> m ()
downloadUpdateDo updHash cps@ConfirmedProposalState {..} = do
    updateServers <- views (lensOf @UpdateParams) upUpdateServers

    logInfo $ sformat ("We are going to start downloading an update for "%build)
              cpsUpdateProposal
    res <- handleAny handleErr $ runExceptT $ do
        let updateVersion = upSoftwareVersion cpsUpdateProposal
        -- It's just a sanity check which must always pass due to the
        -- outside logic. We take only updates for our software and
        -- explicitly request only new updates. This invariant must be
        -- ensure by the caller of 'downloadUpdate'.
        unless (isVersionAppropriate updateVersion) $
            reportFatalError $
            sformat ("Update #"%build%" hasn't been downloaded: "%
                    "its version is not newer than current software "%
                    "software version or it's not for our "%
                    "software at all") updHash

        updPath <- views (lensOf @UpdateParams) upUpdatePath
        whenM (liftIO $ doesFileExist updPath) $
            throwError "There's unapplied update already downloaded"

        logInfo "Downloading update..."
        file <- ExceptT $ downloadHash updateServers updHash <&>
                first (sformat ("Update download (hash "%build%
                                ") has failed: "%stext) updHash)

        logInfo $ "Update was downloaded, saving to " <> show updPath

        liftIO $ BSL.writeFile updPath file
        logInfo $ "Update was downloaded, saved to " <> show updPath
        downloadedMVar <- views (lensOf @UpdateContext) ucDownloadedUpdate
        putMVar downloadedMVar cps
        logInfo "Update MVar filled, wallet is notified"

    whenLeft res logDownloadError
  where
    handleErr e =
        Left (pretty e) <$ reportOrLogW "Update downloading failed: " e
    logDownloadError e =
        logWarning $ sformat
            ("Failed to download update proposal "%build%": "%stext)
            cpsUpdateProposal e
    -- Check that we really should download an update with given
    -- 'SoftwareVersion'.
    isVersionAppropriate :: SoftwareVersion -> Bool
    isVersionAppropriate ver = svAppName ver == svAppName curSoftwareVersion
        && svNumber ver > svNumber curSoftwareVersion

-- Download a file by its hash.
--
-- Tries all servers in turn, fails if none of them work.
downloadHash ::
       (MonadIO m, WithLogger m)
    => [Text]
    -> Hash Raw
    -> m (Either Text LByteString)
downloadHash updateServers h = do
    manager <- liftIO $ newManager tlsManagerSettings

    let -- try all servers in turn until there's a Right
        go errs (serv:rest) = do
            let uri = toString serv <//> showHash h
            logDebug $ "Trying url " <> show uri
            liftIO (downloadUri manager uri h) >>= \case
                Left e -> go (e:errs) rest
                Right r -> return (Right r)

        -- if there were no servers, that's really weird
        go [] [] = return . Left $ "no update servers are known"

        -- if we've tried all servers already, fail
        go errs [] = return . Left $
            sformat ("all update servers failed: "%listJsonIndent 2)
                    (reverse errs)

    go [] updateServers
  where
    showHash :: Hash a -> FilePath
    showHash = toString . B16.encode . BA.convert

-- Download a file and check its hash.
downloadUri :: Manager
            -> String
            -> Hash Raw
            -> IO (Either Text LByteString)
downloadUri manager uri h = do
    request <- setRequestManager manager <$> parseRequest uri
    resp <- httpLBS request
    let (st, stc) = (getResponseStatus resp, getResponseStatusCode resp)
        h' = installerHash (getResponseBody resp)
    return $ if | stc /= 200 -> Left ("error, " <> show st)
                | h /= h'    -> Left "hash mismatch"
                | otherwise  -> Right (getResponseBody resp)

{- TODO

* check timeouts?
* how should we in general deal with e.g. 1B/s download speed?
* if we expect updates to be big, use laziness/conduits (httpLBS isn't lazy,
  despite the “L” in its name)

-}
