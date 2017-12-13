{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Pos.Logic.Full
    ( logicLayerFull
    , LogicWorkMode
    ) where

import           Universum

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Lens (at, to)
import qualified Data.HashMap.Strict as HM
import           Data.Tagged (Tagged (..), tagWith)
import           Ether.Internal (HasLens (..), lensOf)
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logDebug)

import           Pos.Communication (NodeId)
import           Pos.DB.Class (MonadGState (..), MonadBlockDBRead, MonadDBRead)
import qualified Pos.DB.Class as DB (getBlock)
import qualified Pos.DB.Block as DB (getTipBlock)
import qualified Pos.DB.BlockIndex as DB (getHeader, getTipHeader)
import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Block.Types (RecoveryHeaderTag, RecoveryHeader)
import qualified Pos.Block.Logic as DB (getHeadersFromManyTo, getHeadersRange)
import qualified Pos.Block.Network.Logic as Block (handleUnsolicitedHeader)
-- TODO rename these; nothing to do with listening/network.
import           Pos.Delegation.Listeners (DlgListenerConstraint)
import qualified Pos.Delegation.Listeners as Delegation (handlePsk)
import           Pos.Core (HasConfiguration, HeaderHash, Block, BlockVersionData,
                           BlockHeader, ProxySKHeavy, TxAux (..), StakeholderId,
                           addressHash, getCertId, lookupVss)
import           Pos.Core.Context (HasPrimaryKey, getOurStakeholderId)
import           Pos.Core.Ssc (getCommitmentsMap)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Crypto (hash)
import           Pos.Logic.Types (LogicLayer (..), Logic (..), KeyVal (..),
                                  GetTipError (..), GetBlockError (..),
                                  GetBlockHeadersError (..), GetBlockHeaderError (..))
import           Pos.Recovery (MonadRecoveryInfo)
import qualified Pos.Recovery as Recovery
import           Pos.Security.Params (SecurityParams)
import           Pos.Security.Util (shouldIgnorePkAddress)
import           Pos.Slotting (MonadSlots)
import           Pos.Ssc.Logic (sscIsDataUseful, sscProcessCertificate, sscProcessCommitment,
                                sscProcessOpening, sscProcessShares)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..))
import           Pos.Ssc.Mem (sscRunLocalQuery)
import           Pos.Ssc.Toss (TossModifier, tmCertificates, tmCommitments, tmOpenings,
                               tmShares, SscTag (..))
import           Pos.Ssc.Types (ldModifier)
import           Pos.Txp (MemPool (..))
import           Pos.Txp.MemState (getMemPool, JLTxR)
-- TODO rename; nothing to do with listening/network
import           Pos.Txp.Network.Listeners (TxpMode)
import qualified Pos.Txp.Network.Listeners as Txp (handleTxDo)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import qualified Pos.Update.Logic.Local as Update (getLocalProposalNVotes, getLocalVote,
                                                   isProposalNeeded, isVoteNeeded)
import           Pos.Update.Mode (UpdateMode)
import qualified Pos.Update.Network.Listeners as Update (handleProposal, handleVote)
import           Pos.Util.Chrono (NewestFirst, OldestFirst, NE)



type LogicWorkMode ctx m =
    ( HasConfiguration
    , HasBlockConfiguration
    , WithLogger m
    , MonadReader ctx m
    , HasPrimaryKey ctx
    , MonadMask m
    , MonadBlockDBRead m
    , MonadDBRead m
    , MonadGState m
    , MonadRecoveryInfo m
    , MonadSlots ctx m
    , HasLens SecurityParams ctx SecurityParams
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , BlockWorkMode ctx m
    , DlgListenerConstraint ctx m
    , TxpMode ctx m
    , UpdateMode ctx m
    )

-- | A stop-gap full logic layer based on the RealMode. It just uses the
-- monadX constraints to do most of its work.
logicLayerFull
    :: forall ctx m x .
       ( Monad m
       , LogicWorkMode ctx m
       )
    => (JLTxR -> m ())
    -> (LogicLayer m -> m x)
    -> m x
logicLayerFull jsonLogTx k =
    bracket acquire release $ \_ -> do

        -- Delivered monadically but in fact is constant (comes from a
        -- reader context).
        ourStakeholderId <- getOurStakeholderId
        securityParams <- view (lensOf @SecurityParams)

        let
            getBlock :: HeaderHash -> m (Either GetBlockError (Maybe Block))
            getBlock = fmap Right . DB.getBlock

            getTip :: m (Either GetTipError Block)
            getTip = fmap Right DB.getTipBlock

            getTipHeader :: m (Either GetTipError BlockHeader)
            getTipHeader = fmap Right DB.getTipHeader

            getAdoptedBVData :: m BlockVersionData
            getAdoptedBVData = gsAdoptedBVData

            recoveryInProgress :: m Bool
            recoveryInProgress = Recovery.recoveryInProgress

            getBlockHeader :: HeaderHash -> m (Either GetBlockHeaderError (Maybe BlockHeader))
            getBlockHeader = fmap Right . DB.getHeader

            getBlockHeaders
                :: NonEmpty HeaderHash
                -> Maybe HeaderHash
                -> m (Either GetBlockHeadersError (NewestFirst NE BlockHeader))
            getBlockHeaders checkpoints start = do
                result <- runExceptT (DB.getHeadersFromManyTo DB.getHeader checkpoints start)
                either (pure . Left . GetBlockHeadersError) (pure . Right) result

            -- FIXME this should use a list rather than a Maybe over NonEmpty.
            -- An empty list of header hashes is not an error, it just means
            -- none were found.
            getBlockHeaders'
                :: HeaderHash
                -> HeaderHash
                -> m (Either GetBlockHeadersError (Maybe (OldestFirst NE HeaderHash)))
            getBlockHeaders' older newer = do
                outcome <- DB.getHeadersRange DB.getHeader Nothing older newer
                case outcome of
                    Left txt -> pure (Left (GetBlockHeadersError txt))
                    Right it -> pure (Right (Just it))

            postBlockHeader :: BlockHeader -> NodeId -> m ()
            postBlockHeader = Block.handleUnsolicitedHeader

            postPskHeavy :: ProxySKHeavy -> m Bool
            postPskHeavy = Delegation.handlePsk

            postTx = KeyVal
                { toKey = pure . Tagged . hash . taTx . getTxMsgContents
                , handleInv = \(Tagged txId) -> not . HM.member txId . _mpLocalTxs <$> getMemPool
                , handleReq = \(Tagged txId) -> fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> getMemPool
                , handleData = \(TxMsgContents txAux) -> Txp.handleTxDo jsonLogTx txAux
                }

            postUpdate = KeyVal
                { toKey = \(up, _) -> pure . tag $ hash up
                , handleInv = Update.isProposalNeeded . unTagged
                , handleReq = Update.getLocalProposalNVotes . unTagged
                , handleData = Update.handleProposal
                }
              where
                tag = tagWith (Proxy :: Proxy (UpdateProposal, [UpdateVote]))

            postVote = KeyVal
                { toKey = \UpdateVote{..} -> pure $ tag (uvProposalId, uvKey, uvDecision)
                , handleInv = \(Tagged (id, pk, dec)) -> Update.isVoteNeeded id pk dec
                , handleReq = \(Tagged (id, pk, dec)) -> Update.getLocalVote id pk dec
                , handleData = Update.handleVote
                }
              where
                tag = tagWith (Proxy :: Proxy UpdateVote)

            postSscCommitment = postSscCommon
                CommitmentMsg
                (\(MCCommitment (pk, _, _)) -> addressHash pk)
                (\id tm -> MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id)
                (\(MCCommitment comm) -> sscProcessCommitment comm)

            postSscOpening = postSscCommon
                OpeningMsg
                (\(MCOpening key _) -> key)
                (\id tm -> MCOpening id <$> tm ^. tmOpenings . at id)
                (\(MCOpening key open) -> sscProcessOpening key open)

            postSscShares = postSscCommon
                SharesMsg
                (\(MCShares key _) -> key)
                (\id tm -> MCShares id <$> tm ^. tmShares . at id)
                (\(MCShares key shares) -> sscProcessShares key shares)

            postSscVssCert = postSscCommon
                VssCertificateMsg
                (\(MCVssCertificate vc) -> getCertId vc)
                (\id tm -> MCVssCertificate <$> lookupVss id (tm ^. tmCertificates))
                (\(MCVssCertificate cert) -> sscProcessCertificate cert)

            postSscCommon
                :: ( Buildable err, Buildable contents )
                => SscTag
                -> (contents -> StakeholderId)
                -> (StakeholderId -> TossModifier -> Maybe contents)
                -> (contents -> ExceptT err m ())
                -> KeyVal (Tagged contents StakeholderId) contents m
            postSscCommon sscTag contentsToKey toContents processData = KeyVal
                { toKey = pure . tagWith contentsProxy . contentsToKey
                , handleInv = sscIsDataUseful sscTag . unTagged
                , handleReq = \(Tagged addr) -> toContents addr . view ldModifier <$> sscRunLocalQuery ask
                , handleData = \dat -> do
                      let addr = contentsToKey dat
                      -- [CSL-685] TODO: Add here malicious emulation for network
                      -- addresses when TW will support getting peer address
                      -- properly
                      -- Stale comment?
                      handleDataDo dat addr =<< shouldIgnorePkAddress addr
                }
              where
                contentsProxy = (const Proxy :: (contents -> k) -> Proxy contents) contentsToKey
                ignoreFmt =
                    "Malicious emulation: data "%build%" for id "%build%" is ignored"
                handleDataDo dat id shouldIgnore
                    | shouldIgnore = False <$ logDebug (sformat ignoreFmt id dat)
                    | otherwise = sscProcessMessage processData dat
                sscProcessMessage sscProcessMessageDo dat =
                    runExceptT (sscProcessMessageDo dat) >>= \case
                        Left err -> False <$ logDebug (sformat ("Data is rejected, reason: "%build) err)
                        Right () -> return True

            -- TODO implement these proper
            -- I do not know of an existing program which will do what we want:
            -- check the block (header) in isolation; a coarse-but-fast test
            -- to eliminate obviously bogus things. False positive possible,
            -- but no false negatives, like a bloom filter.
            checkBlockHeader = \_ -> pure True
            checkBlock = \_ -> pure True

            logic :: Logic m
            logic = Logic {..}

            runLogicLayer :: forall y . m y -> m y
            runLogicLayer = identity

        k (LogicLayer {..})

  where
    acquire = pure ()
    release = \_ -> pure ()
