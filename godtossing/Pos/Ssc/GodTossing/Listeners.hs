{-# LANGUAGE DataKinds #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Universum

import           Control.Lens                          (at, to)
import           Data.Tagged                           (Tagged (..), tagWith)
import           Formatting                            (build, sformat, (%))
import           Node.Message.Class                    (Message)
import           System.Wlog                           (logDebug)

import           Pos.Binary.Class                      (Bi)
import           Pos.Binary.Crypto                     ()
import           Pos.Binary.GodTossing                 ()
import           Pos.Binary.Infra                      ()
import           Pos.Communication.Types.Protocol      (MsgType (..))
import           Pos.Communication.Limits.Types        (MessageLimited)
import           Pos.Communication.MessagePart         (MessagePart)
import           Pos.Communication.Relay               (DataMsg, InvOrData,
                                                        InvReqDataParams (..),
                                                        MempoolParams (NoMempool),
                                                        Relay (..), ReqMsg)
import           Pos.Core                              (StakeholderId, addressHash)
import           Pos.Security.Util                     (shouldIgnorePkAddress)
import           Pos.Ssc.Class.Listeners               (SscListenersClass (..))
import           Pos.Ssc.Extra                         (sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Core               (getCertId, getCommitmentsMap)
import           Pos.Ssc.GodTossing.LocalData          (ldModifier, sscIsDataUseful,
                                                        sscProcessCertificate,
                                                        sscProcessCommitment,
                                                        sscProcessOpening,
                                                        sscProcessShares)
import           Pos.Ssc.GodTossing.Network.Constraint (GtMessageConstraints)
import           Pos.Ssc.GodTossing.Toss               (GtTag (..), TossModifier,
                                                        tmCertificates, tmCommitments,
                                                        tmOpenings, tmShares)
import           Pos.Ssc.GodTossing.Type               (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message      (MCCommitment (..), MCOpening (..),
                                                        MCShares (..),
                                                        MCVssCertificate (..))
import           Pos.Ssc.Mode                          (SscMode)

instance GtMessageConstraints => SscListenersClass SscGodTossing where
    sscRelays = Tagged
        [ commitmentRelay
        , openingRelay
        , sharesRelay
        , vssCertRelay
        ]

commitmentRelay
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => Relay m
commitmentRelay =
    sscRelay CommitmentMsg
             (\(MCCommitment (pk, _, _)) -> addressHash pk)
             (\id tm -> MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id)
             (\(MCCommitment comm) -> sscProcessCommitment comm)

openingRelay
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => Relay m
openingRelay =
    sscRelay OpeningMsg
             (\(MCOpening k _) -> k)
             (\id tm -> MCOpening id <$> tm ^. tmOpenings . at id)
             (\(MCOpening key open) -> sscProcessOpening key open)

sharesRelay
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => Relay m
sharesRelay =
    sscRelay SharesMsg
             (\(MCShares k _) -> k)
             (\id tm -> MCShares id <$> tm ^. tmShares . at id)
             (\(MCShares key shares) -> sscProcessShares key shares)

vssCertRelay
    :: (GtMessageConstraints, SscMode SscGodTossing ctx m)
    => Relay m
vssCertRelay =
    sscRelay VssCertificateMsg
             (\(MCVssCertificate vc) -> getCertId vc)
             (\id tm -> MCVssCertificate <$> tm ^. tmCertificates . at id)
             (\(MCVssCertificate cert) -> sscProcessCertificate cert)

sscRelay
    :: ( SscMode SscGodTossing ctx m
       , Buildable err
       , Buildable contents
       , Typeable contents
       , MessageLimited (DataMsg contents)
       , Bi (DataMsg contents)
       , MessagePart contents
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqMsg (Tagged contents StakeholderId))
       )
    => GtTag
    -> (contents -> StakeholderId)
    -> (StakeholderId -> TossModifier -> Maybe contents)
    -> (contents -> ExceptT err m ())
    -> Relay m
sscRelay gtTag contentsToKey toContents processData =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = pure . tagWith contentsProxy . contentsToKey
          , handleInv = \_ -> sscIsDataUseful gtTag . unTagged
          , handleReq =
              \_ (Tagged addr) -> toContents addr . view ldModifier <$> sscRunLocalQuery ask
          , handleData = \_ dat -> do
                let addr = contentsToKey dat
                -- [CSL-685] TODO: Add here malicious emulation for network
                -- addresses when TW will support getting peer address
                -- properly
                handleDataDo dat addr =<< shouldIgnorePkAddress addr
          }
  where
    contentsProxy = (const Proxy :: (contents -> k) -> Proxy contents) contentsToKey
    ignoreFmt =
        "Malicious emulation: data " %build % " for id " %build %
        " is ignored"
    handleDataDo dat id shouldIgnore
        | shouldIgnore = False <$ logDebug (sformat ignoreFmt id dat)
        | otherwise = sscProcessMessage processData dat

sscProcessMessage
    :: (SscMode SscGodTossing ctx m, Buildable err)
    => (a -> ExceptT err m ()) -> a -> m Bool
sscProcessMessage sscProcessMessageDo dat =
    runExceptT (sscProcessMessageDo dat) >>= \case
        Left err ->
            False <$ logDebug (sformat ("Data is rejected, reason: " %build) err)
        Right () -> return True
