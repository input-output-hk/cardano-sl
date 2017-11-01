{-# LANGUAGE DataKinds #-}

-- | All SSC listeners.

module Pos.Ssc.Listeners
       ( sscRelays
       ) where

import           Universum

import           Control.Lens                          (at, to)
import           Data.Tagged                           (Tagged (..), tagWith)
import           Formatting                            (build, sformat, (%))
import           Node.Message.Class                    (Message)
import           System.Wlog                           (logDebug)

import           Pos.Binary.Class                      (Bi)
import           Pos.Binary.Crypto                     ()
import           Pos.Binary.Ssc                        ()
import           Pos.Binary.Infra                      ()
import           Pos.Communication.Limits.Types        (MessageLimited)
import           Pos.Communication.Relay               (DataMsg, InvOrData,
                                                        InvReqDataParams (..),
                                                        MempoolParams (NoMempool),
                                                        Relay (..), ReqMsg, ReqOrRes)
import           Pos.Communication.Types.Protocol      (MsgType (..))
import           Pos.Core                              (HasConfiguration, StakeholderId,
                                                        addressHash, getCertId, lookupVss)
import           Pos.Security.Util                     (shouldIgnorePkAddress)
import           Pos.Ssc.Mem                           (sscRunLocalQuery)
import           Pos.Ssc.Configuration                 (HasSscConfiguration)
import           Pos.Ssc.Core                          (getCommitmentsMap)
import           Pos.Ssc.Types                         (ldModifier)
import           Pos.Ssc.LocalData                     (sscIsDataUseful,
                                                        sscProcessCertificate,
                                                        sscProcessCommitment,
                                                        sscProcessOpening,
                                                        sscProcessShares)
import           Pos.Ssc.Toss                          (SscTag (..), TossModifier,
                                                        tmCertificates, tmCommitments,
                                                        tmOpenings, tmShares)
import           Pos.Ssc.Message                       (MCCommitment (..), MCOpening (..),
                                                        MCShares (..),
                                                        MCVssCertificate (..),
                                                        GtMessageConstraints)
import           Pos.Ssc.Mode                          (SscMode)

sscRelays
    :: (GtMessageConstraints, SscMode ctx m)
    => [Relay m]
sscRelays =
    [ commitmentRelay
    , openingRelay
    , sharesRelay
    , vssCertRelay
    ]

commitmentRelay
    :: (GtMessageConstraints, SscMode ctx m)
    => Relay m
commitmentRelay =
    sscRelay CommitmentMsg
             (\(MCCommitment (pk, _, _)) -> addressHash pk)
             (\id tm -> MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id)
             (\(MCCommitment comm) -> sscProcessCommitment comm)

openingRelay
    :: (GtMessageConstraints, SscMode ctx m)
    => Relay m
openingRelay =
    sscRelay OpeningMsg
             (\(MCOpening k _) -> k)
             (\id tm -> MCOpening id <$> tm ^. tmOpenings . at id)
             (\(MCOpening key open) -> sscProcessOpening key open)

sharesRelay
    :: (GtMessageConstraints, SscMode ctx m)
    => Relay m
sharesRelay =
    sscRelay SharesMsg
             (\(MCShares k _) -> k)
             (\id tm -> MCShares id <$> tm ^. tmShares . at id)
             (\(MCShares key shares) -> sscProcessShares key shares)

vssCertRelay
    :: (GtMessageConstraints, SscMode ctx m)
    => Relay m
vssCertRelay =
    sscRelay VssCertificateMsg
             (\(MCVssCertificate vc) -> getCertId vc)
             (\id tm -> MCVssCertificate <$> lookupVss id (tm ^. tmCertificates))
             (\(MCVssCertificate cert) -> sscProcessCertificate cert)

sscRelay
    :: ( SscMode ctx m
       , Buildable err
       , Buildable contents
       , Typeable contents
       , MessageLimited (DataMsg contents)
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
       , HasConfiguration
       , HasSscConfiguration
       )
    => SscTag
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
    :: (SscMode ctx m, Buildable err)
    => (a -> ExceptT err m ()) -> a -> m Bool
sscProcessMessage sscProcessMessageDo dat =
    runExceptT (sscProcessMessageDo dat) >>= \case
        Left err ->
            False <$ logDebug (sformat ("Data is rejected, reason: " %build) err)
        Right () -> return True
