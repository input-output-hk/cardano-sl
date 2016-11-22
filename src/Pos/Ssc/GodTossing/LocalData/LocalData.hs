{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.LocalData
       (
         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
         sscProcessMessage
       ) where

import           Data.Default                       (Default (def))
import           Universum

import           Control.Lens                       (at, use, view, (%=), (.=), (^.))
import qualified Data.HashMap.Strict                as HM
import           Pos.Crypto                         (PublicKey, Share)
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate, MonadSscLD,
                                                     SscLocalDataClass (..),
                                                     sscRunLocalUpdate)
import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.Functions       (checkOpening, checkShares,
                                                     isCommitmentIdx, isOpeningIdx,
                                                     isSharesIdx, verifySignedCommitment)
import           Pos.Ssc.GodTossing.Functions       (filterGtPayload)
import           Pos.Ssc.GodTossing.LocalData.Types (gtGlobalCertificates,
                                                     gtGlobalCommitments,
                                                     gtGlobalOpenings, gtGlobalShares,
                                                     gtLastProcessedSlot,
                                                     gtLocalCertificates,
                                                     gtLocalCommitments, gtLocalOpenings,
                                                     gtLocalShares)
import           Pos.Ssc.GodTossing.Types.Base      (Commitment, CommitmentSignature,
                                                     Opening, VssCertificate)
import           Pos.Ssc.GodTossing.Types.Instance  ()
import           Pos.Ssc.GodTossing.Types.Message   (DataMsg (..))
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types     (GtPayload (..), mdCommitments,
                                                     mdOpenings, mdShares,
                                                     mdVssCertificates)
import           Pos.Types                          (Block, SlotId (..), blockMpc)
import           Pos.Util                           (diffDoubleMap, readerToState)
import           Serokell.Util.Verify               (isVerSuccess)

type LDQuery a = LocalQuery SscGodTossing a
type LDUpdate a = LocalUpdate SscGodTossing a

instance SscLocalDataClass SscGodTossing where
    sscEmptyLocalData = def
    sscGetLocalPayloadQ = getLocalPayload
    sscProcessBlockU = processBlock
    sscProcessNewSlotU = processNewSlot

----------------------------------------------------------------------------
-- Process New Slot
----------------------------------------------------------------------------
-- Should be executed before doing any updates within given slot.
processNewSlot :: SlotId -> LDUpdate ()
processNewSlot si@SlotId {siSlot = slotIdx} = do
    unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
    unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
    unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
    gtLastProcessedSlot .= si

----------------------------------------------------------------------------
-- Ssc Process Message
----------------------------------------------------------------------------
sscProcessMessage ::
       MonadSscLD SscGodTossing m
    => DataMsg -> m Bool
sscProcessMessage = sscRunLocalUpdate  . sscProcessMessageU

sscProcessMessageU :: DataMsg -> LDUpdate Bool
sscProcessMessageU (DMCommitment pk comm)     = processCommitment pk comm
sscProcessMessageU (DMOpening pk open)        = processOpening pk open
sscProcessMessageU (DMShares pk shares)       = processShares pk shares
sscProcessMessageU (DMVssCertificate pk cert) = processVssCertificate pk cert

processCommitment
    :: PublicKey -> (Commitment, CommitmentSignature) -> LDUpdate Bool
processCommitment pk c = do
    epochIdx <- siEpoch <$> use gtLastProcessedSlot
    ok <- readerToState $ and <$> (sequence $ checks epochIdx)
    ok <$ when ok (gtLocalCommitments %= HM.insert pk c)
  where
    checks epochIndex =
        [ pure . isVerSuccess $ verifySignedCommitment pk epochIndex c
        , not . HM.member pk <$> view gtGlobalCommitments
        , not . HM.member pk <$> view gtLocalCommitments
        ]

processOpening :: PublicKey -> Opening -> LDUpdate Bool
processOpening pk o = do
    ok <- readerToState $ and <$> sequence checks
    ok <$ when ok (gtLocalOpenings %= HM.insert pk o)
  where
    checks = [checkOpeningAbsence pk, checkOpeningLastVer pk o]

-- Check that there is no opening from given public key in blocks. It is useful
-- in opening processing.
checkOpeningAbsence :: PublicKey -> LDQuery Bool
checkOpeningAbsence pk =
    (&&) <$> (notMember <$> view gtGlobalOpenings) <*>
    (notMember <$> view gtLocalOpenings)
  where
    notMember = not . HM.member pk

processShares :: PublicKey -> HashMap PublicKey Share -> LDUpdate Bool
processShares pk s
    | null s = pure False
    | otherwise = do
        -- TODO: we accept shares that we already have (but don't add them to
        -- local shares) because someone who sent us those shares might not be
        -- aware of the fact that they are already in the blockchain. On the
        -- other hand, now nodes can send us huge spammy messages and we can't
        -- ban them for that. On the third hand, is this a concern?
        preOk <- readerToState $ checkSharesLastVer pk s
        let mpcProcessSharesDo = do
                globalSharesForPK <-
                    HM.lookupDefault mempty pk <$> use gtGlobalShares
                localSharesForPk <- HM.lookupDefault mempty pk <$> use gtLocalShares
                let s' = s `HM.difference` globalSharesForPK
                let newLocalShares = localSharesForPk `HM.union` s'
                -- Note: size is O(n), but union is also O(n + m), so
                -- it doesn't matter.
                let ok = preOk && (HM.size newLocalShares /= HM.size localSharesForPk)
                ok <$ (when ok $ gtLocalShares . at pk .= Just newLocalShares)
        mpcProcessSharesDo

checkSharesLastVer :: PublicKey -> HashMap PublicKey Share -> LDQuery Bool
checkSharesLastVer pk shares =
    (\comms openings certs -> checkShares comms openings certs pk shares) <$>
    view gtGlobalCommitments <*>
    view gtGlobalOpenings <*>
    view gtGlobalCertificates

-- Apply checkOpening using last version.
checkOpeningLastVer :: PublicKey -> Opening -> LDQuery Bool
checkOpeningLastVer pk opening =
    flip checkOpening (pk, opening) <$> view gtGlobalCommitments

processVssCertificate :: PublicKey -> VssCertificate -> LDUpdate Bool
processVssCertificate pk c = do
    ok <- not . HM.member pk <$> use gtGlobalCertificates
    ok <$ when ok (gtLocalCertificates %= HM.insert pk c)

----------------------------------------------------------------------------
-- Process Block
----------------------------------------------------------------------------
processBlock :: (SscPayload ssc ~ GtPayload) => Block ssc -> LDUpdate ()
processBlock blk = do
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left _  -> return ()
        Right b -> do
            let blockCommitments  = b ^. blockMpc . mdCommitments
                blockOpenings     = b ^. blockMpc . mdOpenings
                blockShares       = b ^. blockMpc . mdShares
                blockCertificates = b ^. blockMpc . mdVssCertificates
            gtLocalCommitments  %= (`HM.difference` blockCommitments)
            -- openings
            gtLocalOpenings  %= (`HM.difference` blockOpenings)
            -- shares
            gtLocalShares  %= (`diffDoubleMap` blockShares)
            -- VSS certificates
            gtLocalCertificates  %= (`HM.difference` blockCertificates)

----------------------------------------------------------------------------
-- Get Local Payload
----------------------------------------------------------------------------
getLocalPayload :: SlotId -> LDQuery GtPayload
getLocalPayload slotId = filterGtPayload slotId <$> getStoredLocalPayload

getStoredLocalPayload :: LDQuery GtPayload
getStoredLocalPayload =
    GtPayload <$> view gtLocalCommitments <*> view gtLocalOpenings <*>
    view gtLocalShares <*> view gtLocalCertificates
