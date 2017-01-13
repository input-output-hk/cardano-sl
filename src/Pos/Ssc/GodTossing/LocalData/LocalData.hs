{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.LocalData
       (
         localOnNewSlot
       , sscIsDataUseful
       , sscProcessMessage
       , getLocalPayload
         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
       ) where

import           Control.Lens                         (Getter, at, use, uses, view, views,
                                                       (%=), (.=))
import           Data.Containers                      (ContainerKey,
                                                       SetContainer (notMember))
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as HS
import           Serokell.Util.Verify                 (isVerSuccess)
import           Universum

import           Pos.Binary.Class                     (Bi)
import           Pos.Crypto                           (Share)
import           Pos.Lrc.Types                        (Richmen, RichmenSet)
import           Pos.Ssc.Class.LocalData              (LocalQuery, LocalUpdate,
                                                       SscLocalDataClass (..))
import           Pos.Ssc.Extra.MonadLD                (MonadSscLD)
import           Pos.Ssc.GodTossing.Functions         (checkCertTTL, checkCommShares,
                                                       checkOpeningMatchesCommitment,
                                                       checkShare, checkShares,
                                                       computeParticipants,
                                                       isCommitmentIdx, isOpeningIdx,
                                                       isSharesIdx,
                                                       verifySignedCommitment)
import           Pos.Ssc.GodTossing.LocalData.Helpers (GtState, gtGlobalCertificates,
                                                       gtGlobalCommitments,
                                                       gtGlobalOpenings, gtGlobalShares,
                                                       gtLastProcessedSlot,
                                                       gtLocalCertificates,
                                                       gtLocalCommitments,
                                                       gtLocalOpenings, gtLocalShares,
                                                       gtRunModify, gtRunRead)
import           Pos.Ssc.GodTossing.LocalData.Types   (ldCertificates, ldCommitments,
                                                       ldLastProcessedSlot, ldOpenings,
                                                       ldShares)
import           Pos.Ssc.GodTossing.Types             (GtGlobalState (..), GtPayload (..),
                                                       InnerSharesMap, SscBi,
                                                       SscGodTossing,
                                                       VssCertificate (vcSigningKey, vcVssKey))
import           Pos.Ssc.GodTossing.Types.Base        (Commitment, Opening,
                                                       SignedCommitment,
                                                       VssCertificatesMap)
import           Pos.Ssc.GodTossing.Types.Message     (GtMsgContents (..), GtMsgTag (..))
import qualified Pos.Ssc.GodTossing.VssCertData       as VCD
import           Pos.Types                            (SlotId (..), StakeholderId,
                                                       addressHash)
import           Pos.Util                             (AsBinary, readerToState)

instance SscBi => SscLocalDataClass SscGodTossing where
    sscGetLocalPayloadQ = getLocalPayload
    sscApplyGlobalStateU = applyGlobal

type LDQuery a = forall m .  MonadReader GtState m => m a
type LDUpdate a = forall m . MonadState GtState m  => m a

----------------------------------------------------------------------------
-- Apply Global State
----------------------------------------------------------------------------
applyGlobal :: Richmen -> GtGlobalState -> LocalUpdate SscGodTossing ()
applyGlobal richmen globalData = do
    let globalCerts = VCD.certs . _gsVssCertificates $ globalData
        participants = computeParticipants richmen globalCerts
        vssPublicKeys = map vcVssKey $ toList participants
        globalCommitments = _gsCommitments globalData
        globalOpenings = _gsOpenings globalData
        globalShares = _gsShares globalData
    let filterCommitments comms =
            foldl' (&) comms $
            [
            -- Remove commitments which are contained already in global state
              (`HM.difference` globalCommitments)
            -- Remove commitments which corresponds to expired certs
            , (`HM.intersection` participants)
            -- If set of certificates changes, set of participants can
            -- change too.  Hence some commitments can become invalid.
            , (HM.filterWithKey (\_ c -> checkCommShares vssPublicKeys c))
            ]
    let filterOpenings opens =
            foldl' (&) opens $
            [ (`HM.difference` globalOpenings)
            , (`HM.intersection` globalCommitments)
            -- Select opening which corresponds its commitment
            , HM.filterWithKey
                  (curry $ checkOpeningMatchesCommitment globalCommitments)
            ]
    let checkCorrectShares pkTo shares = HM.filterWithKey
            (\pkFrom share ->
                 checkShare
                     globalCommitments
                     globalOpenings
                     globalCerts
                     (pkTo, pkFrom, share)) shares
    let filterShares shares =
            foldl' (&) shares $
            [ (`HM.difference` globalShares)
            , (`HM.intersection` participants)
            -- Select shares to nodes which sent commitments
            , map (`HM.intersection` globalCommitments)
            -- Ensure that share sent from pkFrom to pkTo is valid
            , HM.mapWithKey checkCorrectShares
            ]
    ldCommitments  %= filterCommitments
    ldOpenings  %= filterOpenings
    ldShares  %= filterShares
    ldCertificates  %= (`VCD.difference` globalCerts)

----------------------------------------------------------------------------
-- Get Local Payload
----------------------------------------------------------------------------

getLocalPayload :: LocalQuery SscGodTossing (SlotId, GtPayload)
getLocalPayload = do
    s <- view ldLastProcessedSlot
    (s, ) <$> (getPayload (siSlot s) <*> views ldCertificates VCD.certs)
  where
    getPayload slotIdx =
        if | isCommitmentIdx slotIdx ->
               CommitmentsPayload <$> view ldCommitments
           | isOpeningIdx slotIdx -> OpeningsPayload <$> view ldOpenings
           | isSharesIdx slotIdx -> SharesPayload <$> view ldShares
           | otherwise -> pure CertificatesPayload

----------------------------------------------------------------------------
-- Process New Slot
----------------------------------------------------------------------------

-- | Clean-up some data when new slot starts.
localOnNewSlot
    :: MonadSscLD SscGodTossing m
    => RichmenSet -> SlotId -> m ()
localOnNewSlot richmen = gtRunModify . localOnNewSlotU richmen

localOnNewSlotU :: RichmenSet -> SlotId -> LDUpdate ()
localOnNewSlotU richmen si@SlotId {siSlot = slotIdx, siEpoch = epochIdx} = do
    lastSlot <- use gtLastProcessedSlot
    if siEpoch lastSlot /= epochIdx
      then do
        gtLocalCommitments .= mempty
        gtLocalOpenings .= mempty
        gtLocalShares .= mempty
      else do
        unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
        unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
        unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
    gtLocalCertificates %= VCD.setLastKnownSlot si
    gtLocalCertificates %= VCD.filter (`HS.member` richmen)
    gtLastProcessedSlot .= si

----------------------------------------------------------------------------
-- Check knowledge of data
----------------------------------------------------------------------------

-- CHECK: @sscIsDataUseful
-- #sscIsDataUsefulQ

-- | Check whether SSC data with given tag and public key can be added
-- to local data.
sscIsDataUseful
    :: MonadSscLD SscGodTossing m
    => GtMsgTag -> StakeholderId -> m Bool
sscIsDataUseful tag = gtRunRead . sscIsDataUsefulQ tag

-- CHECK: @sscIsDataUsefulQ
-- | Check whether SSC data with given tag and public key can be added
-- to local data.
sscIsDataUsefulQ :: GtMsgTag -> StakeholderId -> LDQuery Bool
sscIsDataUsefulQ CommitmentMsg =
    sscIsDataUsefulImpl gtLocalCommitments gtGlobalCommitments
sscIsDataUsefulQ OpeningMsg =
    sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
sscIsDataUsefulQ SharesMsg =
    sscIsDataUsefulSetImpl gtLocalShares gtGlobalShares
sscIsDataUsefulQ VssCertificateMsg = sscIsCertUsefulImpl
  where
    sscIsCertUsefulImpl addr = do
        loc <- views gtLocalCertificates VCD.certs
        glob <- view gtGlobalCertificates
        lpe <- siEpoch <$> view gtLastProcessedSlot
        if addr `HM.member` loc then pure False
        else pure $ maybe True (== lpe) (VCD.lookupExpiryEpoch addr glob)

type MapGetter a = Getter GtState (HashMap StakeholderId a)
type SetGetter set = Getter GtState set

sscIsDataUsefulImpl :: MapGetter a
                    -> MapGetter a
                    -> StakeholderId
                    -> LDQuery Bool
sscIsDataUsefulImpl localG globalG addr =
    (&&) <$>
        (notMember addr <$> view globalG) <*>
        (notMember addr <$> view localG)

sscIsDataUsefulSetImpl
    :: (SetContainer set, ContainerKey set ~ StakeholderId)
    => MapGetter a -> SetGetter set -> StakeholderId -> LDQuery Bool
sscIsDataUsefulSetImpl localG globalG addr =
    (&&) <$>
        (notMember addr <$> view localG) <*>
        (notMember addr <$> view globalG)

----------------------------------------------------------------------------
-- Ssc Process Message
----------------------------------------------------------------------------

-- | Process message and save it if needed. Result is whether message
-- has been actually added.
sscProcessMessage ::
       (MonadSscLD SscGodTossing m, SscBi)
    => Richmen -> GtMsgContents -> StakeholderId -> m Bool
sscProcessMessage richmen msg =
    gtRunModify . sscProcessMessageU (HS.fromList $ toList richmen) msg

sscProcessMessageU
    :: SscBi
    => RichmenSet
    -> GtMsgContents
    -> StakeholderId
    -> LDUpdate Bool
sscProcessMessageU richmen (MCCommitment comm) addr =
    processCommitment richmen addr comm
sscProcessMessageU _ (MCOpening open) addr =
    processOpening addr open
sscProcessMessageU richmen (MCShares shares) addr =
    processShares richmen addr shares
sscProcessMessageU richmen (MCVssCertificate cert) addr =
    processVssCertificate richmen addr cert

processCommitment
    :: Bi Commitment
    => RichmenSet
    -> StakeholderId
    -> SignedCommitment
    -> LDUpdate Bool
processCommitment richmen addr _ | not (addr `HS.member` richmen) = pure False
processCommitment richmen addr c = do
    certs <- VCD.certs <$> use gtGlobalCertificates
    let participants = certs `HM.intersection` HS.toMap richmen
    let vssPublicKeys = map vcVssKey $ toList participants
    let checks epochIndex vssCerts =
            [ not . HM.member addr <$> view gtGlobalCommitments
            , not . HM.member addr <$> view gtLocalCommitments
            , pure $ addr `HM.member` vssCerts
            , pure . isVerSuccess $ verifySignedCommitment addr epochIndex c
            , pure $ checkCommShares vssPublicKeys c
            ]
    epochIdx <- siEpoch <$> use gtLastProcessedSlot
    ok <- readerToState $ andM $ checks epochIdx certs
    ok <$ when ok (gtLocalCommitments %= HM.insert addr c)

processOpening :: StakeholderId -> Opening -> LDUpdate Bool
processOpening addr o = do
    ok <- readerToState $ andM checks
    ok <$ when ok (gtLocalOpenings %= HM.insert addr o)
  where
    checkAbsence = sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
    checks = [checkAbsence addr, matchOpening addr o]

-- Match opening to commitment from globalCommitments
matchOpening :: StakeholderId -> Opening -> LDQuery Bool
matchOpening addr opening =
    flip checkOpeningMatchesCommitment (addr, opening) <$> view gtGlobalCommitments

processShares :: RichmenSet -> StakeholderId -> InnerSharesMap -> LDUpdate Bool
processShares richmen id s
    | null s = pure False
    | not (id `HS.member` richmen) = pure False
    | otherwise = do
        certs <- VCD.certs <$> use gtGlobalCertificates
        let checks =
              [ not . HM.member id <$> use gtGlobalShares
              , not . HM.member id <$> use gtLocalShares
              , readerToState $ checkSharesLastVer certs id s
              ]
        ok <- andM checks
        ok <$ when ok (gtLocalShares . at id .= Just s)

-- CHECK: #checkShares
checkSharesLastVer
    :: VssCertificatesMap
    -> StakeholderId
    -> HashMap StakeholderId (AsBinary Share)
    -> LDQuery Bool
checkSharesLastVer certs addr shares =
    (\comms openings -> checkShares comms openings certs addr shares) <$>
    view gtGlobalCommitments <*>
    view gtGlobalOpenings

processVssCertificate :: RichmenSet
                      -> StakeholderId
                      -> VssCertificate
                      -> LDUpdate Bool
processVssCertificate richmen addr c
    | (addressHash $ vcSigningKey c) `HS.member` richmen = do
        lpe <- uses gtLastProcessedSlot siEpoch
        ok <- (checkCertTTL lpe c &&) <$> readerToState (sscIsDataUsefulQ VssCertificateMsg addr)
        ok <$ when ok (gtLocalCertificates %= VCD.insert addr c)
    | otherwise = pure False
