
-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Control.Lens                     (at, to)
import           Data.Tagged                      (Tagged (..))
import           Formatting                       (build, sformat, (%))
import           Serokell.Util.Verify             (VerificationRes (..))
import           System.Wlog                      (logDebug)
import           Universum

import           Pos.Binary.Communication         ()
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Relay                 ()
import           Pos.Binary.Ssc                   ()
import           Pos.Communication.Limits         ()
import           Pos.Communication.Message        ()
import           Pos.Communication.Relay          (Relay (..), RelayProxy (..),
                                                   relayListeners, relayStubListeners)
import           Pos.Context                      (getNodeContext)
import           Pos.Security                     (shouldIgnorePkAddress)
import           Pos.Ssc.Class.Listeners          (SscListenersClass (..))
import           Pos.Ssc.Extra                    (sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Core          (getCertId, getCommitmentsMap)
import           Pos.Ssc.GodTossing.LocalData     (ldModifier, sscIsDataUseful,
                                                   sscProcessCertificate,
                                                   sscProcessCommitment,
                                                   sscProcessOpening, sscProcessShares)
import           Pos.Ssc.GodTossing.Toss          (GtTag (..), TossModifier,
                                                   tmCertificates, tmCommitments,
                                                   tmOpenings, tmShares)
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message (GtMsgContents (..), msgContentsTag)
import           Pos.Types                        (StakeholderId, addressHash)
import           Pos.WorkMode                     (WorkMode)

instance SscListenersClass SscGodTossing where
    sscListeners =
        Tagged <$> relayListeners
                    (RelayProxy :: RelayProxy StakeholderId GtTag GtMsgContents)
    sscStubListeners =
        Tagged $ relayStubListeners
                    (RelayProxy :: RelayProxy StakeholderId GtTag GtMsgContents)

instance WorkMode SscGodTossing m =>
         Relay m GtTag StakeholderId GtMsgContents where
    contentsToTag = pure . msgContentsTag
    contentsToKey x =
        pure $
        case x of
            MCShares k _            -> k
            MCOpening k _           -> k
            MCCommitment (pk, _, _) -> addressHash pk
            MCVssCertificate vc     -> getCertId vc

    verifyInvTag       _ = pure VerSuccess
    verifyReqTag       _ = pure VerSuccess
    verifyMempoolTag   _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ = sscIsDataUseful
    handleReq _ tag addr =
        toContents tag addr . view ldModifier <$> sscRunLocalQuery ask
    handleMempool _ _ = pure []
    handleData _ dat = do
        addr <- contentsToKey dat
        -- [CSL-685] TODO: Add here malicious emulation for network addresses
        -- when TW will support getting peer address properly
        handleDataDo addr =<< flip shouldIgnorePkAddress addr <$> getNodeContext
      where
        ignoreFmt =
            "Malicious emulation: data " %build % " for id " %build %
            " is ignored"
        handleDataDo id shouldIgnore
            | shouldIgnore = False <$ logDebug (sformat ignoreFmt id dat)
            | otherwise = sscProcessMessage dat

sscProcessMessage
    :: WorkMode SscGodTossing m
    => GtMsgContents -> m Bool
sscProcessMessage dat =
    runExceptT (sscProcessMessageDo dat) >>= \case
        Left err ->
            False <$ logDebug (sformat ("Data is rejected, reason: " %build) err)
        Right () -> return True
  where
    sscProcessMessageDo (MCCommitment comm)     = sscProcessCommitment comm
    sscProcessMessageDo (MCOpening id open)     = sscProcessOpening id open
    sscProcessMessageDo (MCShares id shares)    = sscProcessShares id shares
    sscProcessMessageDo (MCVssCertificate cert) = sscProcessCertificate cert

toContents :: GtTag -> StakeholderId -> TossModifier -> Maybe GtMsgContents
toContents CommitmentMsg id tm =
    MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id
toContents OpeningMsg id tm = MCOpening id <$> tm ^. tmOpenings . at id
toContents SharesMsg id tm = MCShares id <$> tm ^. tmShares . at id
toContents VssCertificateMsg id tm =
    MCVssCertificate <$> tm ^. tmCertificates . at id

-- toContents CommitmentMsg addr (CommitmentsPayload comm _) =
--     MCCommitment <$> lookup addr (getCommitmentsMap comm)
-- toContents OpeningMsg addr (OpeningsPayload opens _) =
--     MCOpening addr <$> lookup addr opens
-- toContents SharesMsg addr (SharesPayload shares _) =
--     MCShares addr <$> lookup addr shares
-- toContents VssCertificateMsg addr payload =
--     MCVssCertificate <$> lookup addr (_gpCertificates payload)
-- toContents _ _ _ = Nothing
