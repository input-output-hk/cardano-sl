{-# LANGUAGE Rank2Types #-}

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

import           Universum

import           Control.Lens                       ((+=), (.=))
import           Control.Monad.Except               (MonadError (throwError), runExceptT)
import qualified Data.HashMap.Strict                as HM
import           Formatting                         (int, sformat, (%))
import           Serokell.Data.Memory.Units         (Byte)
import           Serokell.Util                      (magnify')
import           System.Wlog                        (WithLogger, logWarning)

import           Pos.Binary.Class                   (biSize)
import           Pos.Binary.GodTossing              ()
import           Pos.Core                           (BlockVersionData (..), EpochIndex,
                                                     SlotId (..), StakeholderId)
import           Pos.Core.Constants                 (memPoolLimitRatio)
import           Pos.DB                             (MonadDB,
                                                     MonadGStateCore (gsAdoptedBVData))
import           Pos.Lrc.Types                      (RichmenStake)
import           Pos.Slotting                       (MonadSlots (getCurrentSlot))
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate,
                                                     SscLocalDataClass (..))
import           Pos.Ssc.Extra                      (MonadSscMem, sscRunGlobalQuery,
                                                     sscRunLocalQuery, sscRunLocalSTM)
import           Pos.Ssc.GodTossing.Core            (GtPayload (..), InnerSharesMap,
                                                     Opening, SignedCommitment,
                                                     VssCertificate, isCommitmentIdx,
                                                     isOpeningIdx, isSharesIdx,
                                                     mkCommitmentsMap,
                                                     mkVssCertificatesMap)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData (..), ldEpoch,
                                                     ldModifier, ldSize)
import           Pos.Ssc.GodTossing.Toss            (GtTag (..), PureToss, TossT,
                                                     TossVerFailure (..),
                                                     evalPureTossWithLogger, evalTossT,
                                                     execTossT, hasCertificateToss,
                                                     hasCommitmentToss, hasOpeningToss,
                                                     hasSharesToss, isGoodSlotForTag,
                                                     normalizeToss, refreshToss,
                                                     tmCertificates, tmCommitments,
                                                     tmOpenings, tmShares,
                                                     verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type            (SscGodTossing)
import           Pos.Ssc.GodTossing.Types           (GtGlobalState)
import           Pos.Ssc.RichmenComponent           (getRichmenSsc)

----------------------------------------------------------------------------
-- Methods from type class
----------------------------------------------------------------------------

instance SscLocalDataClass SscGodTossing where
    sscGetLocalPayloadQ = getLocalPayload
    sscNormalizeU = normalize
    sscNewLocalData =
        GtLocalData mempty . siEpoch . fromMaybe slot0 <$> getCurrentSlot <*>
        pure 1
      where
        slot0 = SlotId 0 minBound

getLocalPayload :: SlotId -> LocalQuery SscGodTossing GtPayload
getLocalPayload SlotId {..} = do
    expectedEpoch <- view ldEpoch
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if expectedEpoch == siEpoch then pure True
        else False <$ logWarning warningMsg
    magnify' ldModifier $
        getPayload isExpected <*> getCertificates isExpected
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
    ldSize .= biSize newModifier

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
    , MonadDB m          -- to get richmen
    , MonadGStateCore m  -- to get block size limit
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
        maxBlockSize <- bvdMaxBlockSize <$> gsAdoptedBVData
        let epoch = ld ^. ldEpoch
        getRichmenSsc epoch >>= \case
            Nothing -> throwError $ TossUnknownRichmen epoch
            Just richmen -> do
                gs <- sscRunGlobalQuery ask
                ExceptT $
                    sscRunLocalSTM $
                    sscProcessDataDo (epoch, richmen) maxBlockSize gs payload
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
    -> Byte
    -> GtGlobalState
    -> GtPayload
    -> m (Either TossVerFailure ())
sscProcessDataDo richmenData maxBlockSize gs payload =
    runExceptT $ do
        storedEpoch <- use ldEpoch
        let givenEpoch = fst richmenData
        let multiRichmen = HM.fromList [richmenData]
        unless (storedEpoch == givenEpoch) $
            throwError $ DifferentEpoches storedEpoch givenEpoch
        let maxMemPoolSize = maxBlockSize * memPoolLimitRatio
        curSize <- use ldSize
        let exhausted = curSize >= maxMemPoolSize
        -- If our mempool is exhausted we drop some data from it.
        oldTM <-
            if | not exhausted -> use ldModifier
               | otherwise ->
                   evalPureTossWithLogger multiRichmen gs .
                   execTossT mempty . refreshToss givenEpoch =<<
                   use ldModifier
        newTM <-
            ExceptT $
            evalPureTossWithLogger multiRichmen gs $
            runExceptT $
            execTossT oldTM $ verifyAndApplyGtPayload (Left storedEpoch) payload
        ldModifier .= newTM
        -- If mempool was exhausted, it's easier to recompute total size.
        -- Otherwise (most common case) we don't want to spend time on it and
        -- just add size of new data.
        -- Note that if data is invalid, all this computation will be
        -- discarded.
        if | exhausted -> ldSize .= biSize newTM
           | otherwise -> ldSize += biSize payload

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
