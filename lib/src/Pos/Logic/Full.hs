{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Logic.Full
    ( LogicWorkMode
    , logicFull
    ) where

import           Universum

import           Control.Lens (at, to)
import qualified Data.HashMap.Strict as HM
import           Data.Tagged (Tagged (..), tagWith)
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logDebug)

import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Configuration (HasBlockConfiguration)
import qualified Pos.Block.Logic as Block
import qualified Pos.Block.Network as Block
import           Pos.Block.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Communication (NodeId)
import           Pos.Core (Block, BlockHeader, BlockVersionData, HasConfiguration, HeaderHash,
                           ProxySKHeavy, StakeholderId, TxAux (..), addressHash, getCertId,
                           lookupVss)
import           Pos.Core.Ssc (getCommitmentsMap)
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Crypto (hash)
import qualified Pos.DB.Block as DB (getTipBlock)
import qualified Pos.DB.BlockIndex as DB (getHeader, getTipHeader)
import           Pos.DB.Class (MonadBlockDBRead, MonadDBRead, MonadGState (..))
import qualified Pos.DB.Class as DB (getBlock)
import           Pos.Delegation.Listeners (DlgListenerConstraint)
import qualified Pos.Delegation.Listeners as Delegation (handlePsk)
import           Pos.Logic.Types (KeyVal (..), Logic (..))
import           Pos.Recovery (MonadRecoveryInfo)
import qualified Pos.Recovery as Recovery
import           Pos.Security.Params (SecurityParams)
import           Pos.Security.Util (shouldIgnorePkAddress)
import           Pos.Slotting (MonadSlots)
import           Pos.Ssc.Logic (sscIsDataUseful, sscProcessCertificate, sscProcessCommitment,
                                sscProcessOpening, sscProcessShares)
import           Pos.Ssc.Mem (sscRunLocalQuery)
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..))
import           Pos.Ssc.Toss (SscTag (..), TossModifier, tmCertificates, tmCommitments, tmOpenings,
                               tmShares)
import           Pos.Ssc.Types (ldModifier)
import           Pos.Txp (MemPool (..))
import           Pos.Txp.MemState (getMemPool, withTxpLocalData)
import           Pos.Txp.Network.Listeners (TxpMode)
import qualified Pos.Txp.Network.Listeners as Txp (handleTxDo)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import qualified Pos.Update.Logic.Local as Update (getLocalProposalNVotes, getLocalVote,
                                                   isProposalNeeded, isVoteNeeded)
import           Pos.Update.Mode (UpdateMode)
import qualified Pos.Update.Network.Listeners as Update (handleProposal, handleVote)
import           Pos.Util.Chrono (NE, NewestFirst, OldestFirst)
import           Pos.Util.JsonLog.Events (JLEvent)
import           Pos.Util.Util (HasLens (..))

-- The full logic layer uses existing pieces from the former monolithic
-- approach, in which there was no distinction between networking and
-- blockchain logic (any piece could use the network via some class constraint
-- on the monad). The class-based approach is not problematic for
-- integration with a diffusion layer, because in practice the concrete
-- monad which satisfies these constraints is a reader form over IO, so we
-- always have
--
--   runIO  :: m x -> IO x
--   liftIO :: IO x -> m x
--
-- thus a diffusion layer which is in IO can be made to work with a logic
-- layer which uses the more complicated monad, and vice-versa.

type LogicWorkMode ctx m =
    ( HasConfiguration
    , HasBlockConfiguration
    , WithLogger m
    , MonadReader ctx m
    , MonadMask m
    , MonadBlockDBRead m
    , MonadDBRead m
    , MonadGState m
    , MonadRecoveryInfo m
    , MonadSlots ctx m
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , BlockWorkMode ctx m
    , DlgListenerConstraint ctx m
    , TxpMode ctx m
    , UpdateMode ctx m
    )

-- | A stop-gap full logic layer based on the RealMode. It just uses the
-- monadX constraints to do most of its work.
logicFull
    :: forall ctx m .
       ( LogicWorkMode ctx m )
    => StakeholderId
    -> SecurityParams
    -> (JLEvent -> m ()) -- ^ JSON log callback. FIXME replace by structured logging solution
    -> Logic m
logicFull ourStakeholderId securityParams jsonLogTx =
    let
        getBlock :: HeaderHash -> m (Maybe Block)
        getBlock = DB.getBlock

        getTip :: m Block
        getTip = DB.getTipBlock

        getTipHeader :: m BlockHeader
        getTipHeader = DB.getTipHeader

        getAdoptedBVData :: m BlockVersionData
        getAdoptedBVData = gsAdoptedBVData

        recoveryInProgress :: m Bool
        recoveryInProgress = Recovery.recoveryInProgress

        getBlockHeader :: HeaderHash -> m (Maybe BlockHeader)
        getBlockHeader = DB.getHeader

        getHashesRange
            :: Maybe Word -- ^ Optional limit on how many to pull in.
            -> HeaderHash
            -> HeaderHash
            -> m (Either Block.GetHashesRangeError (OldestFirst NE HeaderHash))
        getHashesRange = Block.getHashesRange

        getBlockHeaders
            :: Maybe Word -- ^ Optional limit on how many to pull in.
            -> NonEmpty HeaderHash
            -> Maybe HeaderHash
            -> m (Either Block.GetHeadersFromManyToError (NewestFirst NE BlockHeader))
        getBlockHeaders = Block.getHeadersFromManyTo

        getLcaMainChain :: OldestFirst [] BlockHeader -> m (OldestFirst [] BlockHeader)
        getLcaMainChain = Block.lcaWithMainChainSuffix

        postBlockHeader :: BlockHeader -> NodeId -> m ()
        postBlockHeader = Block.handleUnsolicitedHeader

        postPskHeavy :: ProxySKHeavy -> m Bool
        postPskHeavy = Delegation.handlePsk

        postTx = KeyVal
            { toKey = pure . Tagged . hash . taTx . getTxMsgContents
            , handleInv = \(Tagged txId) -> not . HM.member txId . _mpLocalTxs <$> withTxpLocalData getMemPool
            , handleReq = \(Tagged txId) -> fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> withTxpLocalData getMemPool
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
            { toKey = \UnsafeUpdateVote{..} -> pure $ tag (uvProposalId, uvKey, uvDecision)
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
            -> (contents -> m (Either err ()))
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
                sscProcessMessageDo dat >>= \case
                    Left err -> False <$ logDebug (sformat ("Data is rejected, reason: "%build) err)
                    Right () -> return True

    in Logic {..}
