{-# LANGUAGE NamedFieldPuns #-}

-- | Update system related functionality in Auxx.

module Command.Update
       ( vote
       , propose
       ) where

import           Universum

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import           Data.List           ((!!))
import           Data.Time.Units     (convertUnit)
import           Formatting          (sformat, string, (%))
import           Serokell.Util       (sec)
import           System.Wlog         (logDebug)

import           Pos.Binary          (Raw)
import           Pos.Communication   (SendActions, immediateConcurrentConversations,
                                      submitUpdateProposal, submitVote)
import           Pos.Configuration   (HasNodeConfiguration)
import           Pos.Core.Configuration (HasConfiguration, genesisBlockVersionData)
import           Pos.Crypto          (Hash, SignTag (SignUSVote), emptyPassphrase,
                                      encToPublic, hash, hashHexF, safeSign, unsafeHash,
                                      withSafeSigner)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Update          (BlockVersionData (..), BlockVersionModifier (..),
                                      SystemTag, UpId, UpdateData (..), UpdateVote (..),
                                      mkUpdateProposalWSign)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Wallet          (getSecretKeys)

import           Command.Types       (ProposeUpdateParams (..), ProposeUpdateSystem (..))
import           Mode                (CmdCtx (..), AuxxMode, getCmdCtx)

----------------------------------------------------------------------------
-- Vote
----------------------------------------------------------------------------

vote ::
       ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       )
    => SendActions AuxxMode
    -> Int
    -> Bool
    -> UpId
    -> AuxxMode ()
vote sendActions idx decision upid = do
    CmdCtx{ccPeers} <- getCmdCtx
    logDebug $ "Submitting a vote :" <> show (idx, decision, upid)
    skey <- (!! idx) <$> getSecretKeys
    msignature <- withSafeSigner skey (pure emptyPassphrase) $ mapM $
                        \ss -> pure $ safeSign SignUSVote ss (upid, decision)
    case msignature of
        Nothing -> putText "Invalid passphrase"
        Just signature -> do
            let voteUpd = UpdateVote
                    { uvKey        = encToPublic skey
                    , uvProposalId = upid
                    , uvDecision   = decision
                    , uvSignature  = signature
                }
            if null ccPeers
                then putText "Error: no addresses specified"
                else do
                    submitVote (immediateConcurrentConversations sendActions ccPeers) voteUpd
                    putText "Submitted vote"

----------------------------------------------------------------------------
-- Propose
----------------------------------------------------------------------------

propose ::
       ( HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       , HasNodeConfiguration
       )
    => SendActions AuxxMode
    -> ProposeUpdateParams
    -> AuxxMode ()
propose sendActions ProposeUpdateParams{..} = do
    CmdCtx{ccPeers} <- getCmdCtx
    logDebug "Proposing update..."
    skey <- (!! puIdx) <$> getSecretKeys
    let BlockVersionData {..} = genesisBlockVersionData
    let bvm =
            BlockVersionModifier
            { bvmScriptVersion     = puScriptVersion
            , bvmSlotDuration      = convertUnit (sec puSlotDurationSec)
            , bvmMaxBlockSize      = puMaxBlockSize
            , bvmMaxHeaderSize     = bvdMaxHeaderSize
            , bvmMaxTxSize         = bvdMaxTxSize
            , bvmMaxProposalSize   = bvdMaxProposalSize
            , bvmMpcThd            = bvdMpcThd
            , bvmHeavyDelThd       = bvdHeavyDelThd
            , bvmUpdateVoteThd     = bvdUpdateVoteThd
            , bvmUpdateProposalThd = bvdUpdateProposalThd
            , bvmUpdateImplicit    = bvdUpdateImplicit
            , bvmSoftforkRule      = Nothing
            , bvmTxFeePolicy       = Nothing
            , bvmUnlockStakeEpoch  = Nothing
            }
    updateData <- mapM updateDataElement puUpdates
    let udata = HM.fromList updateData
    let whenCantCreate = error . mappend "Failed to create update proposal: "
    withSafeSigner skey (pure emptyPassphrase) $ \case
        Nothing -> putText "Invalid passphrase"
        Just ss -> do
            let updateProposal = either whenCantCreate identity $
                    mkUpdateProposalWSign
                        puBlockVersion
                        bvm
                        puSoftwareVersion
                        udata
                        (mkAttributes ())
                        ss
            if null ccPeers
                then putText "Error: no addresses specified"
                else do
                    submitUpdateProposal (immediateConcurrentConversations sendActions ccPeers) ss updateProposal
                    let id = hash updateProposal
                    putText $
                      sformat ("Update proposal submitted, upId: "%hashHexF) id

updateDataElement :: MonadIO m => ProposeUpdateSystem -> m (SystemTag, UpdateData)
updateDataElement ProposeUpdateSystem{..} = do
    diffHash <- hashFile pusBinDiffPath
    installerHash <- hashFile pusInstallerPath
    pure (pusSystemTag, UpdateData diffHash installerHash dummyHash dummyHash)

dummyHash :: Hash Raw
dummyHash = unsafeHash (0 :: Integer)

hashFile :: MonadIO m => Maybe FilePath -> m (Hash Raw)
hashFile Nothing  = pure dummyHash
hashFile (Just filename) = do
    fileData <- liftIO $ BS.readFile filename
    let h = unsafeHash fileData
    putText $ sformat ("Read file "%string%" succesfuly, its hash: "%hashHexF) filename h
    pure h
