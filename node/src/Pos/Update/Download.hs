{-# LANGUAGE RankNTypes #-}

-- | Logic related to downloading update.

module Pos.Update.Download
       ( downloadUpdate
       ) where

import           Universum

import           Control.Lens            (views)
import           Control.Monad.Except    (ExceptT (..), throwError)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.HashMap.Strict     as HM
import           Ether.Internal          (HasLens (..))
import           Formatting              (build, sformat, stext, (%))
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple     (getResponseBody, getResponseStatus,
                                          getResponseStatusCode, httpLBS, parseRequest,
                                          setRequestManager)
import qualified Serokell.Util.Base16    as B16
import           Serokell.Util.Text      (listJsonIndent)
import           System.Directory        (doesFileExist)
import           System.Wlog             (logInfo, logWarning)

import           Pos.Binary.Update       ()
import           Pos.Core.Types          (SoftwareVersion (..))
import           Pos.Crypto              (Hash, castHash, hash)
import           Pos.Exception           (reportFatalError)
import           Pos.Update.Configuration (curSoftwareVersion, ourSystemTag)
import           Pos.Update.Context      (UpdateContext (..))
import           Pos.Update.Core.Types   (UpdateData (..), UpdateProposal (..))
import           Pos.Update.Mode         (UpdateMode)
import           Pos.Update.Params       (UpdateParams (..))
import           Pos.Update.Poll.Types   (ConfirmedProposalState (..))
import           Pos.Util                ((<//>))
import           Pos.Util.Concurrent     (withMVar)

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
            Nothing -> downloadUpdateDo cps
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

-- Download and save archive update by given `ConfirmedProposalState`
downloadUpdateDo :: UpdateMode ctx m => ConfirmedProposalState -> m ()
downloadUpdateDo cps@ConfirmedProposalState {..} = do
    useInstaller <- views (lensOf @UpdateParams) upUpdateWithPkg
    updateServers <- views (lensOf @UpdateParams) upUpdateServers

    let dataHash = if useInstaller then udPkgHash else udAppDiffHash
        mupdHash = castHash . dataHash <$>
                   HM.lookup ourSystemTag (upData cpsUpdateProposal)

    logInfo $ sformat ("We are going to start downloading an update for "%build)
              cpsUpdateProposal
    res <- runExceptT $ do
        -- It must be enforced by the caller.
        updHash <- maybe (reportFatalError $ sformat
                            ("We are trying to download an update not for our "%
                            "system, update proposal is: "%build)
                            cpsUpdateProposal)
                          pure
                   mupdHash
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
        file <- ExceptT $ liftIO (downloadHash updateServers updHash) <&>
                first (sformat ("Update download (hash "%build%
                                ") has failed: "%stext) updHash)

        liftIO $ BSL.writeFile updPath file
        logInfo "Update was downloaded"
        downloadedMVar <- views (lensOf @UpdateContext) ucDownloadedUpdate
        putMVar downloadedMVar cps
        logInfo "Update MVar filled, wallet is notified"

    whenLeft res logWarning
  where
    -- Check that we really should download an update with given
    -- 'SoftwareVersion'.
    isVersionAppropriate :: SoftwareVersion -> Bool
    isVersionAppropriate ver = svAppName ver == svAppName curSoftwareVersion
        && svNumber ver > svNumber curSoftwareVersion


-- Download a file by its hash.
--
-- Tries all servers in turn, fails if none of them work.
downloadHash :: [Text] -> Hash LByteString -> IO (Either Text LByteString)
downloadHash updateServers h = do
    manager <- newManager tlsManagerSettings

    let -- try all servers in turn until there's a Right
        go errs (serv:rest) = do
            let uri = toString serv <//> showHash h
            downloadUri manager uri h >>= \case
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
            -> Hash LByteString
            -> IO (Either Text LByteString)
downloadUri manager uri h = do
    request <- setRequestManager manager <$> parseRequest uri
    resp <- httpLBS request
    let (st, stc) = (getResponseStatus resp, getResponseStatusCode resp)
        h' = hash (getResponseBody resp)
    return $ if | stc /= 200 -> Left ("error, " <> show st)
                | h /= h'    -> Left "hash mismatch"
                | otherwise  -> Right (getResponseBody resp)

{- TODO
=======

* check timeouts?
* how should we in general deal with e.g. 1B/s download speed?
* if we expect updates to be big, use laziness/conduits (httpLBS isn't lazy,
  despite the “L” in its name)

-}
