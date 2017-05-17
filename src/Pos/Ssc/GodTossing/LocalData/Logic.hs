{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

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

import           Control.Lens                       ((.=))
import           Control.Monad.Except               (MonadError (throwError), runExceptT)
import qualified Data.HashMap.Strict                as HM
import           Formatting                         (int, sformat, (%))
import           Serokell.Util                      (magnify')
import           System.Wlog                        (WithLogger, logWarning)
import           Universum

import           Pos.Binary.Ssc                     ()
import           Pos.Core.Types                     (EpochIndex, SlotId (..),
                                                     StakeholderId)
import           Pos.DB                             (MonadDB)
import qualified Pos.Lrc.DB                         as LrcDB
import           Pos.Lrc.Types                      (RichmenStake)
import           Pos.Slotting                       (MonadSlots (getCurrentSlot))
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate,
                                                     SscLocalDataClass (..))
import           Pos.Ssc.Extra                      (MonadSscMem, sscRunGlobalQuery,
                                                     sscRunLocalQuery, sscRunLocalSTM)
import           Pos.Ssc.GodTossing.Core            (GtPayload (..), InnerSharesMap,
                                                     Opening, SignedCommitment,
                                                     VssCertificate, getCommitmentsMap,
                                                     isCommitmentIdx, isOpeningIdx,
                                                     isSharesIdx, mkCommitmentsMap,
                                                     mkCommitmentsMapUnsafe,
                                                     mkVssCertificatesMap)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData (..), ldEpoch,
                                                     ldModifier)
import           Pos.Ssc.GodTossing.Toss            (GtTag (..), PureToss, TossModifier,
                                                     TossT, TossVerFailure (..),
                                                     evalPureTossWithLogger, evalTossT,
                                                     execTossT, hasCertificateToss,
                                                     hasCommitmentToss, hasOpeningToss,
                                                     hasSharesToss, isGoodSlotForTag,
                                                     normalizeToss, tmCertificates,
                                                     tmCommitments, tmOpenings, tmShares,
                                                     verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type            (SscGodTossing)
import           Pos.Ssc.GodTossing.Types           (GtGlobalState)
import           Pos.Util.Limits                    (bisize, takeFromMap)

----------------------------------------------------------------------------
-- Methods from type class
----------------------------------------------------------------------------

instance SscLocalDataClass SscGodTossing where
    sscGetLocalPayloadQ = getLocalPayload
    sscNormalizeU = normalize
    sscNewLocalData =
        GtLocalData mempty . siEpoch . fromMaybe slot0 <$> getCurrentSlot
      where
        slot0 = SlotId 0 0

-- | Transforms GtPayload to fit under size limit.
stripGtPayload :: (MonadReader TossModifier m) => Word64 -> GtPayload -> m GtPayload
stripGtPayload lim payload | bisize payload <= lim = pure payload
stripGtPayload lim payload = case payload of
    (CertificatesPayload vssmap) ->
        pure $ CertificatesPayload $ takeFromMap lim vssmap
    (CommitmentsPayload (getCommitmentsMap -> comms0) certs0) -> do
        let certs = takeFromMap limCerts certs0
        let comms = takeFromMap (lim - bisize certs) comms0
        pure $ CommitmentsPayload (mkCommitmentsMapUnsafe comms) certs
    (OpeningsPayload openings0 certs0) -> do
        -- TODO this striping procedure should try to select openings that
        -- match commitments
        let certs = takeFromMap limCerts certs0
        let openings = takeFromMap (lim - bisize certs) openings0
        pure $ OpeningsPayload openings certs
    (SharesPayload shares0 certs0) -> do
        let certs = takeFromMap limCerts certs0
        let shares = takeFromMap (lim - bisize certs) shares0
        pure $ SharesPayload shares certs
  where
    limCerts = lim `div` 3 -- certificates are 1/3 less important than everything else
                           -- this is a random choice in fact

getLocalPayload :: Maybe Word64 -> SlotId -> LocalQuery SscGodTossing GtPayload
getLocalPayload limit SlotId {..} = do
    expectedEpoch <- view ldEpoch
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if expectedEpoch == siEpoch then pure True
        else False <$ logWarning warningMsg
    magnify' ldModifier $ do
        naive <- getPayload isExpected <*> getCertificates isExpected
        maybe pure stripGtPayload limit $ naive
  where
    warningFmt = "getLocalPayload: unexpected epoch ("%int%", stored one is "%int%")"
    getPayload True
        | isCommitmentIdx siSlot = CommitmentsPayload <$> view tmCommitments
        | isOpeningIdx siSlot = OpeningsPayload <$> view tmOpenings
        | isSharesIdx siSlot = SharesPayload <$> view tmShares
    getPayload _ = pure CertificatesPayload
    getCertificates isExpected
        | isExpected = view tmCertificates
        | otherwise = pure mempty

normalize :: EpochIndex
          -> RichmenStake
          -> GtGlobalState
          -> LocalUpdate SscGodTossing ()
normalize epoch richmen gs = do
    oldModifier <- use ldModifier
    let richmenData = HM.fromList [(epoch, richmen)]
    newModifier <-
        evalPureTossWithLogger richmenData gs $
        execTossT mempty $ normalizeToss epoch oldModifier
    ldModifier .= newModifier
    ldEpoch .= epoch

----------------------------------------------------------------------------
-- Data processing/retrieval
----------------------------------------------------------------------------

----------------------------------------------------------------------------
---- Inv processing
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to current local data.
sscIsDataUseful
    :: ( WithLogger m
       , MonadDB m
       , MonadSlots m
       , MonadSscMem SscGodTossing m
       )
    => GtTag -> StakeholderId -> m Bool
sscIsDataUseful tag id =
    ifM
        (maybe False (isGoodSlotForTag tag . siSlot) <$> getCurrentSlot)
        (evalTossInMem $ sscIsDataUsefulDo tag)
        (pure False)
  where
    sscIsDataUsefulDo CommitmentMsg     = not <$> hasCommitmentToss id
    sscIsDataUsefulDo OpeningMsg        = not <$> hasOpeningToss id
    sscIsDataUsefulDo SharesMsg         = not <$> hasSharesToss id
    sscIsDataUsefulDo VssCertificateMsg = not <$> hasCertificateToss id
    evalTossInMem
        :: ( WithLogger m
           , MonadDB m
           , MonadSscMem SscGodTossing m
           )
        => TossT PureToss a -> m a
    evalTossInMem action = do
        gs <- sscRunGlobalQuery ask
        ld <- sscRunLocalQuery ask
        let modifier = ld ^. ldModifier
        -- Richmen are irrelevant here.
        evalPureTossWithLogger mempty gs $ evalTossT modifier action

----------------------------------------------------------------------------
---- Data processing
----------------------------------------------------------------------------

type GtDataProcessingMode m =
    ( WithLogger m
    , MonadDB m  -- to get richmen
    , MonadSlots m
    , MonadSscMem SscGodTossing m
    , MonadError TossVerFailure m
    )

-- | Process 'SignedCommitment' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCommitment
    :: forall m.
       GtDataProcessingMode m
    => SignedCommitment -> m ()
sscProcessCommitment comm =
    sscProcessData CommitmentMsg $
    CommitmentsPayload (mkCommitmentsMap [comm]) mempty

-- | Process 'Opening' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessOpening
    :: GtDataProcessingMode m
    => StakeholderId -> Opening -> m ()
sscProcessOpening id opening =
    sscProcessData OpeningMsg $
    OpeningsPayload (HM.fromList [(id, opening)]) mempty

-- | Process 'InnerSharesMap' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessShares
    :: GtDataProcessingMode m
    => StakeholderId -> InnerSharesMap -> m ()
sscProcessShares id shares =
    sscProcessData SharesMsg $ SharesPayload (HM.fromList [(id, shares)]) mempty

-- | Process 'VssCertificate' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCertificate
    :: GtDataProcessingMode m
    => VssCertificate -> m ()
sscProcessCertificate cert =
    sscProcessData VssCertificateMsg $
    CertificatesPayload (mkVssCertificatesMap [cert])

sscProcessData
    :: forall m.
       GtDataProcessingMode m
    => GtTag -> GtPayload -> m ()
sscProcessData tag payload =
    generalizeExceptT $ do
        getCurrentSlot >>= checkSlot
        ld <- sscRunLocalQuery ask
        let epoch = ld ^. ldEpoch
        LrcDB.getRichmenSsc epoch >>= \case
            Nothing -> throwError $ TossUnknownRichmen epoch
            Just richmen -> do
                gs <- sscRunGlobalQuery ask
                ExceptT $
                    sscRunLocalSTM $
                    sscProcessDataDo (epoch, richmen) gs payload
  where
    generalizeExceptT action = either throwError pure =<< runExceptT action
    checkSlot Nothing = throwError CurrentSlotUnknown
    checkSlot (Just si@SlotId {..})
        | isGoodSlotForTag tag siSlot = pass
        | CommitmentMsg <- tag = throwError $ NotCommitmentPhase si
        | OpeningMsg <- tag = throwError $ NotOpeningPhase si
        | SharesMsg <- tag = throwError $ NotSharesPhase si
        | otherwise = pass

sscProcessDataDo
    :: (MonadState GtLocalData m, WithLogger m)
    => (EpochIndex, RichmenStake)
    -> GtGlobalState
    -> GtPayload
    -> m (Either TossVerFailure ())
sscProcessDataDo richmenData gs payload =
    runExceptT $ do
        storedEpoch <- use ldEpoch
        let givenEpoch = fst richmenData
        let multiRichmen = HM.fromList [richmenData]
        unless (storedEpoch == givenEpoch) $
            throwError $ DifferentEpoches storedEpoch givenEpoch
        oldTM <- use ldModifier
        newTM <-
            ExceptT $
            evalPureTossWithLogger multiRichmen gs $
            runExceptT $
            execTossT oldTM $ verifyAndApplyGtPayload (Left storedEpoch) payload
        ldModifier .= newTM

----------------------------------------------------------------------------
-- Clean-up
----------------------------------------------------------------------------

-- | Clean-up some data when new slot starts.
-- This function is only needed for garbage collection, it doesn't affect
-- validity of local data.
-- Currently it does nothing, but maybe later we'll decide to do clean-up.
localOnNewSlot
    :: MonadSscMem SscGodTossing m
    => SlotId -> m ()
localOnNewSlot _ = pass
-- unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
-- unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
-- unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
