{-# LANGUAGE NamedFieldPuns #-}

-- | Update system related functionality in Auxx.

module Command.Update
       ( vote
       , propose
       , hashInstaller
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import           Formatting (sformat, string, (%))
import           System.Wlog (CanLog, HasLoggerName, logDebug, logError, logInfo)

import           Pos.Binary (Raw)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Client.Update.Network (submitUpdateProposal, submitVote)
import           Pos.Core (protocolMagic)
import           Pos.Crypto (Hash, emptyPassphrase, hash, hashHexF, unsafeHash, withSafeSigner,
                             withSafeSigners)
import           Pos.Diffusion.Types (Diffusion (..))
import           Pos.Exception (reportFatalError)
import           Pos.Update (SystemTag, UpId, UpdateData (..), installerHash, mkUpdateProposalWSign,
                             mkUpdateVoteSafe)

import           Lang.Value (ProposeUpdateParams (..), ProposeUpdateSystem (..))
import           Mode (MonadAuxxMode)
import           Repl (PrintAction)

----------------------------------------------------------------------------
-- Vote
----------------------------------------------------------------------------

vote
    :: MonadAuxxMode m
    => Diffusion m
    -> Int
    -> Bool
    -> UpId
    -> m ()
vote diffusion idx decision upid = do
    logDebug $ "Submitting a vote :" <> show (idx, decision, upid)
    skey <- (!! idx) <$> getSecretKeysPlain
    mbVoteUpd <- withSafeSigner skey (pure emptyPassphrase) $ mapM $ \signer ->
        pure $ mkUpdateVoteSafe protocolMagic signer upid decision
    case mbVoteUpd of
        Nothing -> logError "Invalid passphrase"
        Just voteUpd -> do
            submitVote diffusion voteUpd
            logInfo "Submitted vote"

----------------------------------------------------------------------------
-- Propose, hash installer
----------------------------------------------------------------------------

propose
    :: MonadAuxxMode m
    => Diffusion m
    -> ProposeUpdateParams
    -> m UpId
propose diffusion ProposeUpdateParams{..} = do
    logDebug "Proposing update..."
    skey <- (!! puSecretKeyIdx) <$> getSecretKeysPlain
    updateData <- mapM updateDataElement puUpdates
    let udata = HM.fromList updateData
    skeys <- if not puVoteAll then pure [skey]
             else getSecretKeysPlain
    withSafeSigners skeys (pure emptyPassphrase) $ \ss -> do
        unless (length skeys == length ss) $
            reportFatalError $ "Number of safe signers: " <> show (length ss) <>
                               ", expected " <> show (length skeys)
        let publisherSS = ss !! if not puVoteAll then 0 else puSecretKeyIdx
        let updateProposal =
                mkUpdateProposalWSign
                    protocolMagic
                    puBlockVersion
                    puBlockVersionModifier
                    puSoftwareVersion
                    udata
                    def
                    publisherSS
        let upid = hash updateProposal
        submitUpdateProposal diffusion ss updateProposal
        if not puVoteAll then
            putText (sformat ("Update proposal submitted, upId: "%hashHexF) upid)
        else
            putText (sformat ("Update proposal submitted along with votes, upId: "%hashHexF) upid)
        return upid

updateDataElement :: MonadAuxxMode m => ProposeUpdateSystem -> m (SystemTag, UpdateData)
updateDataElement ProposeUpdateSystem{..} = do
    diffHash <- hashFile pusBinDiffPath
    pkgHash <- hashFile pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash pkgHash dummyHash dummyHash)

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: (CanLog m, HasLoggerName m, MonadIO m) => Maybe FilePath -> m (Hash Raw)
hashFile Nothing  = pure dummyHash
hashFile (Just filename) = do
    fileData <- liftIO $ BSL.readFile filename
    let h = installerHash fileData
    logInfo $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h

hashInstaller :: MonadIO m => PrintAction m -> FilePath -> m ()
hashInstaller printAction path = do
    h <- installerHash <$> liftIO (BSL.readFile path)
    printAction $ sformat ("Hash of installer '"%string%"' is "%hashHexF) path h
