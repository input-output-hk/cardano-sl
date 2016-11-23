{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.LocalData
       (
         sscIsDataUseful
       , sscProcessMessage
         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
       ) where

import           Data.Default                       (Default (def))
import           Universum

import           Control.Lens                       (at, use, view, (%=), (.=), (^.))
import qualified Data.HashMap.Strict                as HM
import           Pos.Crypto                         (PublicKey, Share)
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate, MonadSscLD,
                                                     SscLocalDataClass (..),
                                                     sscRunLocalQuery, sscRunLocalUpdate)
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
import           Pos.Ssc.GodTossing.Types.Message   (DataMsg (..), MsgTag (..))
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types     (GtPayload (..), mdCommitments,
                                                     mdOpenings, mdShares,
                                                     mdVssCertificates)
import           Pos.Types                          (Block, SlotId (..))
import           Pos.Util                           (diffDoubleMap, readerToState)
import           Serokell.Util.Verify               (isVerSuccess)
import           Pos.State.Storage.Types (ProcessBlockRes (..))

type LDQuery a = LocalQuery SscGodTossing a
type LDUpdate a = LocalUpdate SscGodTossing a

instance SscLocalDataClass SscGodTossing where
    sscEmptyLocalData = def
    sscGetLocalPayloadQ = getLocalPayload
    sscApplyGlobalPayloadU = applyGlobal
    sscProcessNewSlotU = processNewSlot

----------------------------------------------------------------------------
-- Process New Slot
----------------------------------------------------------------------------
-- Should be executed before doing any updates within given slot.
processNewSlot :: SlotId -> LDUpdate ()
processNewSlot si@SlotId {siSlot = slotIdx} = do
    when (slotIdx == 0) $ do
        gtGlobalCommitments .= mempty
        gtGlobalOpenings    .= mempty
        gtGlobalShares      .= mempty
    unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
    unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
    unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
    gtLastProcessedSlot .= si

----------------------------------------------------------------------------
-- Check knowledge of data
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to local data.
sscIsDataUseful
    :: MonadSscLD SscGodTossing m
    => MsgTag -> PublicKey -> m Bool
sscIsDataUseful tag = sscRunLocalQuery . sscIsDataUsefulQ tag

sscIsDataUsefulQ :: MsgTag -> PublicKey -> LDQuery Bool
sscIsDataUsefulQ CommitmentMsg pk =
    not . or <$>
    sequence
        [ (HM.member pk <$> view gtLocalCommitments)
        , (HM.member pk <$> view gtGlobalCommitments)
        ]
sscIsDataUsefulQ OpeningMsg pk =
    not . or <$>
    sequence
        [ (HM.member pk <$> view gtLocalOpenings)
        , (HM.member pk <$> view gtGlobalOpenings)
        ]
sscIsDataUsefulQ SharesMsg pk =
    not . or <$>
    sequence
        [ (HM.member pk <$> view gtLocalShares)
        , (HM.member pk <$> view gtGlobalShares)
        ]
sscIsDataUsefulQ VssCertificateMsg pk =
    not . or <$>
    sequence
        [ (HM.member pk <$> view gtLocalCertificates)
        , (HM.member pk <$> view gtGlobalCertificates)
        ]

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
-- Apply Block
----------------------------------------------------------------------------

applyGlobal :: (SscPayload ssc ~ GtPayload) => ProcessBlockRes ssc -> SscPayload ssc -> LDUpdate ()
applyGlobal (PBRabort _) _ = return ()
applyGlobal _ payload = do
    let
        payloadCommitments = _mdCommitments payload
        payloadOpenings = _mdOpenings payload
        payloadShares = _mdShares payload
        payloadCert = _mdVssCertificates payload
    gtLocalCommitments  %= (`HM.difference` payloadCommitments)
    gtLocalOpenings  %= (`HM.difference` payloadOpenings)
    gtLocalShares  %= (`diffDoubleMap` payloadShares)
    gtLocalCertificates  %= (`HM.difference` payloadCert)

    gtGlobalCommitments .= payloadCommitments
    gtGlobalOpenings .= payloadOpenings
    gtGlobalShares .= payloadShares
    gtGlobalCertificates .= payloadCert

----------------------------------------------------------------------------
-- Get Local Payload
----------------------------------------------------------------------------
getLocalPayload :: SlotId -> LDQuery GtPayload
getLocalPayload slotId = filterGtPayload slotId <$> getStoredLocalPayload

getStoredLocalPayload :: LDQuery GtPayload
getStoredLocalPayload =
    GtPayload <$> view gtLocalCommitments <*> view gtLocalOpenings <*>
    view gtLocalShares <*> view gtLocalCertificates


