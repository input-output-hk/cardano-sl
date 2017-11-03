{-# LANGUAGE NamedFieldPuns #-}

-- | Update system related functionality in Auxx.

module Command.Update
       ( vote
       , propose
       , hashInstaller
       ) where

import           Universum

import qualified Data.ByteString.Lazy     as BSL
import           Data.Default             (def)
import qualified Data.HashMap.Strict      as HM
import           Data.List                ((!!))
import           Formatting               (sformat, string, (%))
import           System.Wlog              (logDebug, logError, logInfo)

import           Pos.Binary               (Raw)
import           Pos.Client.KeyStorage    (getSecretKeysPlain)
import           Pos.Communication        (SendActions, immediateConcurrentConversations,
                                           submitUpdateProposal, submitVote)
import           Pos.Configuration        (HasNodeConfiguration)
import           Pos.Core.Configuration   (HasConfiguration)
import           Pos.Crypto               (Hash, SignTag (SignUSVote), emptyPassphrase,
                                           encToPublic, hash, hashHexF, safeSign,
                                           unsafeHash, withSafeSigner, withSafeSigners)
import           Pos.Exception            (reportFatalError)
import           Pos.Infra.Configuration  (HasInfraConfiguration)
import           Pos.Update               (SystemTag, UpId, UpdateData (..),
                                           UpdateVote (..), installerHash,
                                           mkUpdateProposalWSign)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo     (HasCompileInfo)

import           Lang.Value               (ProposeUpdateParams (..),
                                           ProposeUpdateSystem (..))
import           Mode                     (AuxxMode, CmdCtx (..), getCmdCtx)

----------------------------------------------------------------------------
-- Vote
----------------------------------------------------------------------------

vote
    :: ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       , HasCompileInfo
       )
    => SendActions AuxxMode
    -> Int
    -> Bool
    -> UpId
    -> AuxxMode ()
vote sendActions idx decision upid = do
    CmdCtx{ccPeers} <- getCmdCtx
    logDebug $ "Submitting a vote :" <> show (idx, decision, upid)
    skey <- (!! idx) <$> getSecretKeysPlain
    msignature <- withSafeSigner skey (pure emptyPassphrase) $ mapM $
                        \ss -> pure $ safeSign SignUSVote ss (upid, decision)
    case msignature of
        Nothing -> logError "Invalid passphrase"
        Just signature -> do
            let voteUpd = UpdateVote
                    { uvKey        = encToPublic skey
                    , uvProposalId = upid
                    , uvDecision   = decision
                    , uvSignature  = signature
                }
            if null ccPeers
                then logError "Error: no addresses specified"
                else do
                    submitVote (immediateConcurrentConversations sendActions ccPeers) voteUpd
                    logInfo "Submitted vote"

----------------------------------------------------------------------------
-- Propose, hash installer
----------------------------------------------------------------------------

propose
    :: ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       , HasCompileInfo
       )
    => SendActions AuxxMode
    -> ProposeUpdateParams
    -> AuxxMode UpId
propose sendActions ProposeUpdateParams{..} = do
    CmdCtx{ccPeers} <- getCmdCtx
    logDebug "Proposing update..."
    skey <- (!! puSecretKeyIdx) <$> getSecretKeysPlain
    updateData <- mapM updateDataElement puUpdates
    let udata = HM.fromList updateData
    let whenCantCreate = error . mappend "Failed to create update proposal: "
    skeys <- if not puVoteAll then pure [skey]
             else getSecretKeysPlain
    withSafeSigners skeys (pure emptyPassphrase) $ \ss -> do
        unless (length skeys == length ss) $
            reportFatalError $ "Number of safe signers: " <> show (length ss) <>
                               ", expected " <> show (length skeys)
        let publisherSS = ss !! if not puVoteAll then 0 else puSecretKeyIdx
        let updateProposal = either whenCantCreate identity $
                mkUpdateProposalWSign
                    puBlockVersion
                    puBlockVersionModifier
                    puSoftwareVersion
                    udata
                    def
                    publisherSS
        if null ccPeers
            then reportFatalError "Error: no addresses specified"
            else do
                let upid = hash updateProposal
                let enqueue = immediateConcurrentConversations sendActions ccPeers
                submitUpdateProposal enqueue ss updateProposal
                if not puVoteAll then
                    putText (sformat ("Update proposal submitted, upId: "%hashHexF) upid)
                else
                    putText (sformat ("Update proposal submitted along with votes, upId: "%hashHexF) upid)
                return upid

updateDataElement :: ProposeUpdateSystem -> AuxxMode (SystemTag, UpdateData)
updateDataElement ProposeUpdateSystem{..} = do
    diffHash <- hashFile pusBinDiffPath
    pkgHash <- hashFile pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash pkgHash dummyHash dummyHash)

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: Maybe FilePath -> AuxxMode (Hash Raw)
hashFile Nothing  = pure dummyHash
hashFile (Just filename) = do
    fileData <- liftIO $ BSL.readFile filename
    let h = installerHash fileData
    logInfo $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h

hashInstaller :: FilePath -> AuxxMode ()
hashInstaller path = do
    h <- installerHash <$> liftIO (BSL.readFile path)
    logInfo $ sformat ("Hash of installer '"%string%"' is "%hashHexF) path h
