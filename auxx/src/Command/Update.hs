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

import           Pos.Binary (Raw)
import           Pos.Chain.Update (SystemTag, UpId, UpdateData (..),
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Client.KeyStorage (getSecretKeysPlain)
import           Pos.Client.Update.Network (submitUpdateProposal, submitVote)
import           Pos.Core.Exception (traceFatalError)
import           Pos.Crypto (Hash, ProtocolMagic, emptyPassphrase, hash,
                     hashHexF, unsafeHash, withSafeSigner, withSafeSigners)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Network.Update.Download (installerHash)
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logError, logInfo)

import           Lang.Value (ProposeUpdateParams (..), ProposeUpdateSystem (..))
import           Mode (MonadAuxxMode)
import           Repl (PrintAction)

----------------------------------------------------------------------------
-- Vote
----------------------------------------------------------------------------

vote
    :: MonadAuxxMode m
    => TraceNamed m
    -> ProtocolMagic
    -> Diffusion m
    -> Int
    -> Bool
    -> UpId
    -> m ()
vote logTrace pm diffusion idx decision upid = do
    logDebug logTrace $ "Submitting a vote :" <> show (idx, decision, upid)
    skey <- (!! idx) <$> getSecretKeysPlain
    mbVoteUpd <- withSafeSigner skey (pure emptyPassphrase) $ mapM $ \signer ->
        pure $ mkUpdateVoteSafe pm signer upid decision
    case mbVoteUpd of
        Nothing -> logError logTrace "Invalid passphrase"
        Just voteUpd -> do
            submitVote diffusion voteUpd
            logInfo logTrace "Submitted vote"

----------------------------------------------------------------------------
-- Propose, hash installer
----------------------------------------------------------------------------

propose
    :: MonadAuxxMode m
    => TraceNamed m
    -> ProtocolMagic
    -> Diffusion m
    -> ProposeUpdateParams
    -> m UpId
propose logTrace pm diffusion ProposeUpdateParams{..} = do
    logDebug logTrace "Proposing update..."
    skey <- (!! puSecretKeyIdx) <$> getSecretKeysPlain
    updateData <- mapM (updateDataElement logTrace) puUpdates
    let udata = HM.fromList updateData
    skeys <- if not puVoteAll then pure [skey]
             else getSecretKeysPlain
    withSafeSigners skeys (pure emptyPassphrase) $ \ss -> do
        unless (length skeys == length ss) $
            traceFatalError logTrace $ "Number of safe signers: " <> show (length ss) <>
                               ", expected " <> show (length skeys)
        let publisherSS = ss !! if not puVoteAll then 0 else puSecretKeyIdx
        let updateProposal =
                mkUpdateProposalWSign
                   pm
                    puBlockVersion
                    puBlockVersionModifier
                    puSoftwareVersion
                    udata
                    def
                    publisherSS
        let upid = hash updateProposal
        submitUpdateProposal logTrace pm diffusion ss updateProposal
        if not puVoteAll then
            putText (sformat ("Update proposal submitted, upId: "%hashHexF) upid)
        else
            putText (sformat ("Update proposal submitted along with votes, upId: "%hashHexF) upid)
        return upid

updateDataElement
    :: MonadAuxxMode m
    => TraceNamed m
    -> ProposeUpdateSystem
    -> m (SystemTag, UpdateData)
updateDataElement logTrace ProposeUpdateSystem{..} = do
    diffHash <- hashFile logTrace pusBinDiffPath
    pkgHash <- hashFile logTrace pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash pkgHash dummyHash dummyHash)

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: MonadIO m => TraceNamed m -> Maybe FilePath -> m (Hash Raw)
hashFile _ Nothing  = pure dummyHash
hashFile logTrace (Just filename) = do
    fileData <- liftIO $ BSL.readFile filename
    let h = installerHash fileData
    logInfo logTrace $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h

hashInstaller :: MonadIO m => PrintAction m -> FilePath -> m ()
hashInstaller printAction path = do
    h <- installerHash <$> liftIO (BSL.readFile path)
    printAction $ sformat ("Hash of installer '"%string%"' is "%hashHexF) path h
