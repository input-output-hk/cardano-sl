{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Ssc.GodTossing.Server
       (
         announceCommitment
       , announceCommitments
       , announceOpening
       , announceOpenings
       , announceShares
       , announceSharesMulti
       , announceVssCertificate
       , announceVssCertificates
       ) where

import           Data.List.NonEmpty                (NonEmpty)
import           Formatting                        (sformat, (%))
import           Serokell.Util.Text                (listJson)
import           System.Wlog                       (logDebug)
import           Universum

import           Pos.Communication.Methods         (announceSsc)
import           Pos.Crypto                        (PublicKey, Share)
import           Pos.Ssc.GodTossing.Base           (Opening, SignedCommitment,
                                                    VssCertificate)
import           Pos.Ssc.GodTossing.Instance.Type  (SscGodTossing)
import           Pos.Ssc.GodTossing.Instance.Types ()
import           Pos.Ssc.GodTossing.Types          (GtMessage (..))
import           Pos.WorkMode                      (WorkMode)

-- TODO: add statlogging for everything, see e.g. announceTxs
announceCommitment :: WorkMode SscGodTossing m => PublicKey -> SignedCommitment -> m ()
announceCommitment pk comm = announceCommitments $ pure (pk, comm)

announceCommitments
    :: WorkMode SscGodTossing m
    => NonEmpty (PublicKey, SignedCommitment) -> m ()
announceCommitments comms = do
    -- TODO: should we show actual commitments?
    logDebug $
        sformat ("Announcing commitments from: "%listJson) $ map fst comms
    announceSsc $ DSCommitments comms

announceOpening :: WorkMode SscGodTossing m => PublicKey -> Opening -> m ()
announceOpening pk open = announceOpenings $ pure (pk, open)

announceOpenings :: WorkMode SscGodTossing m => NonEmpty (PublicKey, Opening) -> m ()
announceOpenings openings = do
    -- TODO: should we show actual openings?
    logDebug $
        sformat ("Announcing openings from: "%listJson) $ map fst openings
    announceSsc $ DSOpenings openings

announceShares :: WorkMode SscGodTossing m => PublicKey -> HashMap PublicKey Share -> m ()
announceShares pk shares = announceSharesMulti $ pure (pk, shares)

announceSharesMulti
    :: WorkMode SscGodTossing m
    => NonEmpty (PublicKey, HashMap PublicKey Share) -> m ()
announceSharesMulti shares = do
    -- TODO: should we show actual shares?
    logDebug $
        sformat ("Announcing shares from: "%listJson) $ map fst shares
    announceSsc $ DSSharesMulti shares

announceVssCertificate
    :: WorkMode SscGodTossing m
    => PublicKey -> VssCertificate -> m ()
announceVssCertificate pk cert = announceVssCertificates $ pure (pk, cert)

announceVssCertificates
    :: WorkMode SscGodTossing m
    => NonEmpty (PublicKey, VssCertificate) -> m ()
announceVssCertificates certs = do
    -- TODO: should we show actual certificates?
    logDebug $ sformat
        ("Announcing VSS certificates from: "%listJson) $ map fst certs
    announceSsc $ DSVssCertificates certs
