{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to downloading update.

module Pos.Update.Download
       ( downloadUpdate
       , downloadHash
       ) where

import           Control.Concurrent.STM  (modifyTVar')
import           Control.Monad.Except    (ExceptT (..), throwError)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.HashMap.Strict     as HM
import qualified Data.Set                as S
import qualified Ether
import           Formatting              (build, sformat, stext, (%))
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple     (getResponseBody, getResponseStatus,
                                          getResponseStatusCode, httpLBS, parseRequest,
                                          setRequestManager)
import qualified Serokell.Util.Base16    as B16
import           Serokell.Util.Text      (listJsonIndent)
import           System.Directory        (doesFileExist)
import           System.Wlog             (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Update       ()
import           Pos.Constants           (appSystemTag, curSoftwareVersion)
import           Pos.Core.Types          (SoftwareVersion (..))
import           Pos.Crypto              (Hash, castHash, hash)
import           Pos.Update.Context      (UpdateContext (..))
import           Pos.Update.Core.Types   (UpId, UpdateData (..), UpdateProposal (..))
import           Pos.Update.Mode         (UpdateMode)
import           Pos.Update.Params       (UpdateParams (..))
import           Pos.Update.Poll.Types   (ConfirmedProposalState (..))
import           Pos.Util                ((<//>))

showHash :: Hash a -> FilePath
showHash = toString . B16.encode . BA.convert

-- CSL-887: if we're downloading update not for `cardano-sl`,
-- but e. g. for `daedalus`, how do we check that version is new?
versionIsNew :: SoftwareVersion -> Bool
versionIsNew ver = svAppName ver /= svAppName curSoftwareVersion
    || svNumber ver > svNumber curSoftwareVersion

-- TODO Now we suppose there is no more than one update at every moment.
-- | Determine whether to download update and download it if needed.
downloadUpdate :: forall m . UpdateMode m => ConfirmedProposalState -> m ()
downloadUpdate cst@ConfirmedProposalState {..} = do
    unlessM (liftIO . doesFileExist =<< Ether.asks' upUpdatePath) $ do
        downSetVar <- Ether.asks' ucDownloadingUpdates
        let upId = hash cpsUpdateProposal
        whenM (tryPutToSet downSetVar upId) $
            downloadUpdateDo cst
            `finally` (atomically $ modifyTVar' downSetVar (S.delete upId))
  where
    -- Whether to start downloading?
    tryPutToSet :: TVar (Set UpId) -> UpId -> m Bool
    tryPutToSet downSetVar upId = atomically $ do
        downSet <- readTVar downSetVar
        if S.member upId downSet then pure False
        else True <$ writeTVar downSetVar (S.insert upId downSet)

-- | Download and save archive update by given `ConfirmedProposalState`
downloadUpdateDo :: UpdateMode m => ConfirmedProposalState -> m ()
downloadUpdateDo cst@ConfirmedProposalState {..} = do
    logDebug "Update downloading triggered"
    useInstaller <- Ether.asks' upUpdateWithPkg
    updateServers <- Ether.asks' upUpdateServers

    let dataHash = if useInstaller then udPkgHash else udAppDiffHash
        mupdHash = castHash . dataHash <$>
                   HM.lookup appSystemTag (upData cpsUpdateProposal)

    res <- runExceptT $ do
        updHash <- maybe (throwError "This update is not for our system")
                   pure mupdHash
        let updateVersion = upSoftwareVersion cpsUpdateProposal
        unless (versionIsNew updateVersion) $
            throwError $ sformat ("Update #"%build%" hasn't been downloaded: \
                                  \current software version is newer than \
                                  \update version") updHash

        updPath <- Ether.asks' upUpdatePath
        whenM (liftIO $ doesFileExist updPath) $
            throwError "There's unapplied update already downloaded"

        logInfo "Downloading update..."
        file <- ExceptT $ liftIO (downloadHash updateServers updHash) <&>
                first (sformat ("Update download (hash "%build%
                                ") has failed: "%stext) updHash)

        liftIO $ BSL.writeFile updPath file
        logInfo "Update was downloaded"
        sm <- Ether.asks' ucUpdateSemaphore
        putMVar sm cst
        logInfo "Update MVar filled, wallet is notified"

    whenLeft res logWarning

-- | Download a file by its hash.
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
