{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines methods which operate on GtLocalData.

module Pos.Ssc.GodTossing.LocalData.Logic
       (
         -- * 'Inv|Req|Data' processing.
         sscIsDataUseful
       , sscProcessCommitment
       , sscProcessOpening
       , sscProcessShares
       , sscProcessCertificate

         -- * Garbage collection worker
       , localOnNewSlot

         -- * Instances
         -- ** instance SscLocalDataClass SscGodTossing
       ) where

import           Control.Lens                       (Getter, at, to, (%=), (.=))
import           Control.Monad.Except               (MonadError (throwError), runExceptT)
import           Control.Monad.State                (get)
import           Data.Containers                    (ContainerKey,
                                                     SetContainer (notMember))
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import           Data.List.NonEmpty                 (NonEmpty ((:|)))
import           Formatting                         (int, sformat, (%))
import           Serokell.Util.Verify               (isVerSuccess)
import           System.Wlog                        (WithLogger, logWarning)
import           Universum

import           Pos.Binary.Ssc                     ()
import           Pos.Context                        (WithNodeContext)
import           Pos.DB                             (MonadDB)
import           Pos.Lrc.Types                      (RichmenSet)
import           Pos.Slotting                       (MonadSlots (getCurrentSlot))
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate,
                                                     SscLocalDataClass (..))
import           Pos.Ssc.Extra                      (MonadSscMem, sscRunGlobalQuery,
                                                     sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Core            (CommitmentsMap (getCommitmentsMap),
                                                     GtPayload (..), InnerSharesMap,
                                                     Opening, SignedCommitment,
                                                     VssCertificate (vcSigningKey, vcVssKey),
                                                     checkCertTTL, checkCommShares,
                                                     checkShare, checkShares, diffCommMap,
                                                     getCertId, insertSignedCommitment,
                                                     intersectCommMapWith,
                                                     isCommitmentIdx, isOpeningIdx,
                                                     isSharesIdx, vcExpiryEpoch,
                                                     verifySignedCommitment)
import           Pos.Ssc.GodTossing.Functions       (computeParticipants,
                                                     getStableCertsPure)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData (..), ldEpoch,
                                                     ldModifier)
import           Pos.Ssc.GodTossing.Toss            (MonadTossRead (..), PureToss,
                                                     TossModifier, TossT,
                                                     TossVerErrorTag (..),
                                                     TossVerFailure (..),
                                                     checkOpeningMatchesCommitment,
                                                     evalPureTossWithLogger, evalTossT,
                                                     tmCertificates, tmCommitments,
                                                     tmOpenings, tmShares)
import           Pos.Ssc.GodTossing.Type            (SscGodTossing)
import           Pos.Ssc.GodTossing.Types           (GtGlobalState, isGoodSlotForTag,
                                                     _gsCommitments, _gsOpenings,
                                                     _gsShares, _gsVssCertificates)
import           Pos.Ssc.GodTossing.Types.Message   (GtMsgContents (..), GtMsgTag (..))
import qualified Pos.Ssc.GodTossing.VssCertData     as VCD
import           Pos.Types                          (EpochIndex, SlotId (..),
                                                     StakeholderId, addressHash)
import           Pos.Util                           (magnify')

----------------------------------------------------------------------------
-- Methods from type class
----------------------------------------------------------------------------

instance SscLocalDataClass SscGodTossing where
    sscGetLocalPayloadQ = getLocalPayload
    sscNormalizeU = normalize
    sscNewLocalData = GtLocalData mempty . siEpoch <$> getCurrentSlot

getLocalPayload :: SlotId -> LocalQuery SscGodTossing GtPayload
getLocalPayload SlotId {..} = do
    expectedEpoch <- view ldEpoch
    let warningFmt =
            "getLocalPayload: unexpected epoch (" %int % ", stored one is " %int %
            ")"
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if | expectedEpoch == siEpoch -> return True
           | otherwise -> False <$ logWarning warningMsg
    magnify' ldModifier $ getPayload isExpected <*> getCertificates isExpected
  where
    getPayload isExpected
        | isCommitmentIdx siSlot =
            CommitmentsPayload <$> getPayloadDo isExpected tmCommitments
        | isOpeningIdx siSlot =
            OpeningsPayload <$> getPayloadDo isExpected tmOpenings
        | isSharesIdx siSlot =
            SharesPayload <$> getPayloadDo isExpected tmShares
        | otherwise = pure CertificatesPayload
    getPayloadDo
        :: (MonadReader TossModifier m, Monoid a)
        => Bool -> Getter TossModifier a -> m a
    getPayloadDo isExpected getter
        | isExpected = view getter
        | otherwise = pure mempty
    getCertificates isExpected
        | isExpected = view tmCertificates
        | otherwise = pure mempty

normalize :: EpochIndex -> RichmenSet -> GtGlobalState -> LocalUpdate SscGodTossing ()
normalize = notImplemented
    -- gtLocalCertificates %= VCD.setLastKnownSlot si
    -- gtLocalCertificates %= VCD.filter (`HS.member` richmen)
    -- gtEpoch .= epochIdx

----------------------------------------------------------------------------
-- Data processing/retrieval
----------------------------------------------------------------------------

----------------------------------------------------------------------------
---- Helpers
----------------------------------------------------------------------------

evalTossInMem
    :: ( WithLogger m
       , MonadDB SscGodTossing m
       , WithNodeContext kek m
       , MonadSscMem SscGodTossing m
       )
    => TossT PureToss a -> m a
evalTossInMem action = do
    gs <- sscRunGlobalQuery ask
    ld <- sscRunLocalQuery ask
    let modifier = ld ^. ldModifier
    evalPureTossWithLogger undefined gs $ evalTossT modifier action

----------------------------------------------------------------------------
---- Inv processing
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to current local data.
sscIsDataUseful
    :: ( WithLogger m
       , MonadDB SscGodTossing m
       , WithNodeContext kek m
       , MonadSlots m
       , MonadSscMem SscGodTossing m
       )
    => GtMsgTag -> StakeholderId -> m Bool
sscIsDataUseful tag id =
    ifM
        (isGoodSlotForTag tag . siSlot <$> getCurrentSlot)
        (evalTossInMem $ sscIsDataUsefulDo tag)
        (pure False)
  where
    sscIsDataUsefulDo CommitmentMsg     = isNothing <$> getCommitment id
    sscIsDataUsefulDo OpeningMsg        = not <$> hasOpening id
    sscIsDataUsefulDo SharesMsg         = not <$> hasShares id
    sscIsDataUsefulDo VssCertificateMsg = not <$> hasCertificate id

----------------------------------------------------------------------------
---- Req processing
----------------------------------------------------------------------------

-- TODO: move logic from 'Listeners' here.

----------------------------------------------------------------------------
---- Data processing
----------------------------------------------------------------------------

type GtDataProcessingMode m =
    ( WithLogger m
    , MonadDB SscGodTossing m  -- to get richmen
    , WithNodeContext SscGodTossing m  -- to get richmen
    , MonadSlots m
    , MonadSscMem SscGodTossing m
    , MonadError TossVerFailure m
    )

-- | Process 'SignedCommitment' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCommitment
    :: GtDataProcessingMode m
    => SignedCommitment -> m ()
sscProcessCommitment = notImplemented

-- | Process 'Opening' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessOpening
    :: GtDataProcessingMode m
    => StakeholderId -> Opening -> m ()
sscProcessOpening = notImplemented

-- | Process 'InnerSharesMap' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessShares
    :: GtDataProcessingMode m
    => StakeholderId -> InnerSharesMap -> m ()
sscProcessShares = notImplemented

-- | Process 'VssCertificate' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCertificate
    :: GtDataProcessingMode m
    => VssCertificate -> m ()
sscProcessCertificate = notImplemented

----------------------------------------------------------------------------
-- Clean-up
----------------------------------------------------------------------------

-- | Clean-up some data when new slot starts.
-- This function is only needed for garbage collection, it doesn't affect
-- validity of local data.
localOnNewSlot
    :: MonadSscMem SscGodTossing m
    => SlotId -> m ()
localOnNewSlot _ = const pass notImplemented
-- localOnNewSlotU si@SlotId {siSlot = slotIdx, siEpoch = epochIdx} = do
--     storedEpoch <- use gtEpoch
--     if storedEpoch /= epochIdx
--         then do
--             gtLocalCommitments .= mempty
--             gtLocalOpenings .= mempty
--             gtLocalShares .= mempty
--         else do
--             unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
--             unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
--             unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty

----------------------------------------------------------------------------
-- Obsolete
----------------------------------------------------------------------------

-- applyGlobal :: EpochIndex -> RichmenSet -> GtGlobalState -> LocalUpdate SscGodTossing ()
-- applyGlobal newEpoch richmenSet globalData = do
--     let globalCerts = VCD.certs . _gsVssCertificates $ globalData
--         participants = computeParticipants richmenSet globalCerts
--         globalCommitments = _gsCommitments globalData
--         -- globalComms = getCommitmentsMap globalCommitments
--         globalOpenings = _gsOpenings globalData
--         globalShares = _gsShares globalData
--         intersectWithPs = intersectCommMapWith identity
--     let filterCommitments comms =
--             foldl' (&) comms $
--             [
--             -- Remove commitments which are contained already in global state
--               (`diffCommMap` globalCommitments)
--             -- Remove commitments which corresponds to expired certs
--             , (`intersectWithPs` participants)
--             ]
--     let filterOpenings opens =
--             foldl' (&) opens $
--             [ (`HM.difference` globalOpenings)
--             , (`HM.intersection` getCommitmentsMap globalCommitments)
--             -- Select opening which corresponds its commitment
--             , HM.filterWithKey
--                   (curry $ checkOpeningMatchesCommitment globalCommitments)
--             ]
--     let checkCorrectShares pkTo shares = HM.filterWithKey
--             (\pkFrom share ->
--                  checkShare
--                      globalCommitments
--                      globalOpenings
--                      globalCerts
--                      (pkTo, pkFrom, share)) shares
--     let filterShares shares =
--             foldl' (&) shares $
--             [ (`HM.difference` globalShares)
--             , (`HM.intersection` participants)
--             -- Select shares to nodes which sent commitments
--             , map (`HM.intersection` getCommitmentsMap globalCommitments)
--             -- Ensure that share sent from pkFrom to pkTo is valid
--             , HM.mapWithKey checkCorrectShares
--             ]
--     ldCommitments  %= filterCommitments
--     ldOpenings  %= filterOpenings
--     ldShares  %= filterShares
--     ldCertificates  %= (`VCD.difference` globalCerts)
--     ldEpoch .= newEpoch

-- ----------------------------------------------------------------------------
-- -- Check knowledge of data
-- ----------------------------------------------------------------------------

-- -- CHECK: @sscIsDataUsefulQ
-- -- | Check whether SSC data with given tag and public key can be added
-- -- to local data.
-- sscIsDataUsefulQ :: GtMsgTag -> StakeholderId -> LDQuery Bool
-- sscIsDataUsefulQ CommitmentMsg =
--     sscIsDataUsefulImpl
--         (gtLocalCommitments . to getCommitmentsMap)
--         (gtGlobalCommitments . to getCommitmentsMap)
-- sscIsDataUsefulQ OpeningMsg =
--     sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
-- sscIsDataUsefulQ SharesMsg =
--     sscIsDataUsefulSetImpl gtLocalShares gtGlobalShares
-- sscIsDataUsefulQ VssCertificateMsg = sscIsCertUsefulImpl
--   where
--     sscIsCertUsefulImpl addr = do
--         loc <- VCD.certs <$> view gtLocalCertificates
--         glob <- VCD.certs <$> view gtGlobalCertificates
--         return $ (not $ addr `HM.member` loc) && (not $ addr `HM.member` glob)

-- type MapGetter a = Getter GtState (HashMap StakeholderId a)
-- type SetGetter set = Getter GtState set

-- sscIsDataUsefulImpl :: MapGetter a
--                     -> MapGetter a
--                     -> StakeholderId
--                     -> LDQuery Bool
-- sscIsDataUsefulImpl localG globalG addr =
--     andM [ notMember addr <$> view globalG
--          , notMember addr <$> view localG ]

-- sscIsDataUsefulSetImpl
--     :: (SetContainer set, ContainerKey set ~ StakeholderId)
--     => MapGetter a -> SetGetter set -> StakeholderId -> LDQuery Bool
-- sscIsDataUsefulSetImpl localG globalG addr =
--     andM [ notMember addr <$> view localG
--          , notMember addr <$> view globalG ]

-- ----------------------------------------------------------------------------
-- -- Ssc Process Message
-- ----------------------------------------------------------------------------

-- -- | Process message and save it if needed. Result is whether message
-- -- has been actually added.
-- sscProcessMessage ::
--        (MonadSscMem SscGodTossing m)
--     => (EpochIndex, RichmenSet) -> GtMsgContents -> m (Either TossVerFailure ())
-- sscProcessMessage richmen msg =
--     gtRunModify $ runExceptT $
--     sscProcessMessageU richmen msg

-- sscProcessMessageU
--     :: (EpochIndex, RichmenSet)
--     -> GtMsgContents
--     -> LDProcess ()
-- sscProcessMessageU (richmenEpoch, richmen) msg = do
--     epochIdx <- use gtEpoch
--     when (epochIdx /= richmenEpoch) $
--            throwError $ DifferentEpoches richmenEpoch epochIdx
--     case msg of
--         MCCommitment     comm -> processCommitment richmen comm
--         MCOpening id open     -> processOpening id open
--         MCShares  id shares   -> processShares richmen id shares
--         MCVssCertificate cert -> processVssCertificate richmen cert

-- runChecks :: MonadError TossVerFailure m => [(m Bool, TossVerFailure)] -> m ()
-- runChecks checks =
--     whenJustM (findFalseM checks) $ throwError . snd
--   where
--     -- mmm, velosipedik
--     findFalseM []     = pure Nothing
--     findFalseM (x:xs) = ifM (fst x) (findFalseM xs) (pure $ Just x)

-- -- | Convert (ReaderT s) to any (MonadState s)
-- readerTToState
--     :: MonadState s m
--     => ReaderT s m a -> m a
-- readerTToState rdr = get >>= runReaderT rdr

-- processCommitment
--     :: RichmenSet
--     -> SignedCommitment
--     -> LDProcess ()
-- processCommitment richmen c = do
--     epochIdx <- use gtEpoch
--     stableCerts <- getStableCertsPure epochIdx <$> use gtGlobalCertificates
--     let participants = stableCerts `HM.intersection` HS.toMap richmen
--     let vssPublicKeys = map vcVssKey $ toList participants
--     let checks epochIndex =
--             [ (not . HM.member id . getCommitmentsMap <$> view gtGlobalCommitments
--               , tossEx CommitmentAlreadySent)
--             , (not . HM.member id . getCommitmentsMap <$> view gtLocalCommitments
--               , tossEx CommitmentAlreadySent)
--             , (pure $ id `HM.member` participants
--               , tossEx CommitingNoParticipants)
--             , (pure . isVerSuccess $ verifySignedCommitment epochIndex c
--               , tossEx CommitmentInvalid)
--             , (pure $ checkCommShares vssPublicKeys c
--               , tossEx CommSharesOnWrongParticipants)
--             ]
--     readerTToState $ runChecks $ checks epochIdx
--     gtLocalCommitments %= insertSignedCommitment c
--   where
--     id = addressHash $ view _1 c
--     tossEx = flip TossVerFailure (id:|[])

-- processOpening :: StakeholderId -> Opening -> LDProcess ()
-- processOpening id o = do
--     readerTToState $ runChecks checks
--     gtLocalOpenings %= HM.insert id o
--   where
--     tossEx = flip TossVerFailure (id:|[])
--     checks = [ (checkAbsence id, tossEx OpeningAlreadySent)
--              , (matchOpening id o, tossEx OpeningNotMatchCommitment)
--              ]

--     checkAbsence = sscIsDataUsefulSetImpl gtLocalOpenings gtGlobalOpenings
--     -- Match opening to commitment from globalCommitments
--     matchOpening sid opening =
--         flip checkOpeningMatchesCommitment (sid, opening) <$> view gtGlobalCommitments

-- -- CHECK: #checkShares
-- processShares :: RichmenSet -> StakeholderId -> InnerSharesMap -> LDProcess ()
-- processShares richmen id s = do
--     epochIdx <- use gtEpoch
--     stableCerts <- getStableCertsPure epochIdx <$> use gtGlobalCertificates
--     let checks =
--           [ (pure $ not (id `HS.member` richmen), tossEx SharesNotRichmen)
--           , (sscIsDataUsefulQ SharesMsg id, tossEx SharesAlreadySent)
--           , (checkSharesDo stableCerts s, tossEx DecrSharesNotMatchCommitment)
--           ]
--     readerTToState $ runChecks checks
--     gtLocalShares . at id .= Just s
--   where
--     tossEx = flip TossVerFailure (id:|[])
--     checkSharesDo certs shares = do
--         comms    <- view gtGlobalCommitments
--         openings <- view gtGlobalOpenings
--         pure (checkShares comms openings certs id shares)

-- processVssCertificate :: RichmenSet
--                       -> VssCertificate
--                       -> LDProcess ()
-- processVssCertificate richmen c = do
--     lpe <- use gtEpoch
--     let checks =
--           [ ( pure $ (addressHash $ vcSigningKey c) `HS.member` richmen,
--               tossEx CertificateNotRichmen)
--           , ( pure $ checkCertTTL lpe c, CertificateInvalidTTL certSingleton)
--           , ( sscIsDataUsefulQ VssCertificateMsg id, tossEx CertificateAlreadySent)
--           ]
--     readerTToState $ runChecks checks
--     gtLocalCertificates %= VCD.insert c
--   where
--     id = getCertId c
--     certSingleton = (c, vcExpiryEpoch c):|[]
--     tossEx = flip TossVerFailure (id:|[])
