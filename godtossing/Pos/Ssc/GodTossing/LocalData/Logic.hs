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
import           Serokell.Util                      (magnify')
import           System.Wlog                        (WithLogger, logWarning)

import           Pos.Binary.Class                   (biSize)
import           Pos.Binary.GodTossing              ()
import           Pos.Core                           (BlockVersionData (..), EpochIndex,
                                                     HasCoreConstants, SlotId (..),
                                                     StakeholderId, blkSecurityParamM)
import           Pos.Core.Constants                 (memPoolLimitRatio)
import           Pos.DB                             (MonadDBRead,
                                                     MonadGState (gsAdoptedBVData))
import           Pos.Lrc.Types                      (RichmenStakes)
import           Pos.Slotting                       (MonadSlots (getCurrentSlot))
import           Pos.Ssc.Class.LocalData            (LocalQuery, LocalUpdate,
                                                     SscLocalDataClass (..),
                                                     SscLocalDataTag)
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
                                                     supplyPureTossEnv, tmCertificates,
                                                     tmCommitments, tmOpenings, tmShares,
                                                     verifyAndApplyGtPayload)
import           Pos.Ssc.GodTossing.Type            (SscGodTossing)
import           Pos.Ssc.GodTossing.Types           (GtGlobalState)
import           Pos.Ssc.RichmenComponent           (getRichmenSsc)
import           Pos.Util.Util                      (HasLens (..))

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
    expectedEpoch <- view (lensOf @SscLocalDataTag . ldEpoch)
    blkSecurityParam <- blkSecurityParamM
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if expectedEpoch == siEpoch then pure True
        else False <$ logWarning warningMsg
    magnify' (lensOf @SscLocalDataTag . ldModifier) $
        getPayload blkSecurityParam isExpected <*> getCertificates isExpected
  where
    warningFmt = "getLocalPayload: unexpected epoch ("%int%", stored one is "%int%")"
    getPayload k True
        | isCommitmentIdx k siSlot = CommitmentsPayload <$> view tmCommitments
        | isOpeningIdx k siSlot = OpeningsPayload <$> view tmOpenings
        | isSharesIdx k siSlot = SharesPayload <$> view tmShares
    getPayload _ _ = pure CertificatesPayload
    getCertificates isExpected
        | isExpected = view tmCertificates
        | otherwise = pure mempty

normalize :: (EpochIndex, RichmenStakes)
          -> BlockVersionData
          -> GtGlobalState
          -> LocalUpdate SscGodTossing ()
normalize (epoch, stake) bvd gs = do
    oldModifier <- use ldModifier
    let multiRichmen = HM.fromList [(epoch, stake)]
    newModifier <-
        evalPureTossWithLogger gs $ supplyPureTossEnv (multiRichmen, bvd) $
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
       , MonadIO m
       , MonadSlots m
       , MonadSscMem SscGodTossing ctx m
       , HasCoreConstants ctx
       )
    => GtTag -> StakeholderId -> m Bool
sscIsDataUseful tag id = do
    k <- blkSecurityParamM
    (maybe False (isGoodSlotForTag k tag . siSlot) <$> getCurrentSlot) >>= \case
        True -> (evalTossInMem $ sscIsDataUsefulDo tag)
        False -> (pure False)
  where
    sscIsDataUsefulDo CommitmentMsg     = not <$> hasCommitmentToss id
    sscIsDataUsefulDo OpeningMsg        = not <$> hasOpeningToss id
    sscIsDataUsefulDo SharesMsg         = not <$> hasSharesToss id
    sscIsDataUsefulDo VssCertificateMsg = not <$> hasCertificateToss id
    evalTossInMem ::
           ( WithLogger m
           , MonadIO m
           , MonadSscMem SscGodTossing ctx m
           , HasCoreConstants ctx
           )
        => TossT PureToss a
        -> m a
    evalTossInMem action = do
        gs <- sscRunGlobalQuery ask
        modifier <-
            sscRunLocalQuery $ view (lensOf @SscLocalDataTag . ldModifier)
        evalPureTossWithLogger gs $ evalTossT modifier action

----------------------------------------------------------------------------
---- Data processing
----------------------------------------------------------------------------

type GtDataProcessingMode ctx m =
    ( WithLogger m
    , MonadIO m      -- STM at least
    , MonadDBRead m  -- to get richmen
    , MonadGState m  -- to get block size limit
    , MonadSlots m
    , MonadSscMem SscGodTossing ctx m
    , MonadError TossVerFailure m
    , HasCoreConstants ctx
    )

-- | Process 'SignedCommitment' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCommitment
    :: forall ctx m.
       GtDataProcessingMode ctx m
    => SignedCommitment -> m ()
sscProcessCommitment comm =
    sscProcessData CommitmentMsg $
    CommitmentsPayload (mkCommitmentsMap [comm]) mempty

-- | Process 'Opening' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessOpening
    :: GtDataProcessingMode ctx m
    => StakeholderId -> Opening -> m ()
sscProcessOpening id opening =
    sscProcessData OpeningMsg $
    OpeningsPayload (HM.fromList [(id, opening)]) mempty

-- | Process 'InnerSharesMap' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessShares
    :: GtDataProcessingMode ctx m
    => StakeholderId -> InnerSharesMap -> m ()
sscProcessShares id shares =
    sscProcessData SharesMsg $ SharesPayload (HM.fromList [(id, shares)]) mempty

-- | Process 'VssCertificate' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCertificate
    :: GtDataProcessingMode ctx m
    => VssCertificate -> m ()
sscProcessCertificate cert =
    sscProcessData VssCertificateMsg $
    CertificatesPayload (mkVssCertificatesMap [cert])

sscProcessData
    :: forall ctx m.
       GtDataProcessingMode ctx m
    => GtTag -> GtPayload -> m ()
sscProcessData tag payload =
    generalizeExceptT $ do
        k <- blkSecurityParamM
        getCurrentSlot >>= checkSlot k
        epoch <- sscRunLocalQuery $ view (lensOf @SscLocalDataTag . ldEpoch)
        bvd <- gsAdoptedBVData
        getRichmenSsc epoch >>= \case
            Nothing -> throwError $ TossUnknownRichmen epoch
            Just richmen -> do
                gs <- sscRunGlobalQuery ask
                ExceptT $
                    sscRunLocalSTM $
                    sscProcessDataDo (epoch, richmen) bvd gs payload
  where
    generalizeExceptT action = either throwError pure =<< runExceptT action
    checkSlot _ Nothing = throwError CurrentSlotUnknown
    checkSlot k (Just si@SlotId {..})
        | isGoodSlotForTag k tag siSlot = pass
        | CommitmentMsg <- tag = throwError $ NotCommitmentPhase si
        | OpeningMsg <- tag = throwError $ NotOpeningPhase si
        | SharesMsg <- tag = throwError $ NotSharesPhase si
        | otherwise = pass

sscProcessDataDo ::
       ( MonadState GtLocalData m
       , WithLogger m
       , MonadReader ctx m
       , HasCoreConstants ctx
       )
    => (EpochIndex, RichmenStakes)
    -> BlockVersionData
    -> GtGlobalState
    -> GtPayload
    -> m (Either TossVerFailure ())
sscProcessDataDo richmenData bvd gs payload =
    runExceptT $ do
        storedEpoch <- use ldEpoch
        let givenEpoch = fst richmenData
        let multiRichmen = HM.fromList [richmenData]
        unless (storedEpoch == givenEpoch) $
            throwError $ DifferentEpoches storedEpoch givenEpoch
        let maxMemPoolSize = bvdMaxBlockSize bvd * memPoolLimitRatio
        curSize <- use ldSize
        let exhausted = curSize >= maxMemPoolSize
        -- If our mempool is exhausted we drop some data from it.
        oldTM <-
            if | not exhausted -> use ldModifier
               | otherwise ->
                   evalPureTossWithLogger gs .
                   supplyPureTossEnv (multiRichmen, bvd) .
                   execTossT mempty . refreshToss givenEpoch =<<
                   use ldModifier
        newTM <-
            ExceptT $
            evalPureTossWithLogger gs $
            supplyPureTossEnv (multiRichmen, bvd) $
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
    :: MonadSscMem SscGodTossing ctx m
    => SlotId -> m ()
localOnNewSlot _ = pass
-- unless (isCommitmentIdx slotIdx) $ gtLocalCommitments .= mempty
-- unless (isOpeningIdx slotIdx) $ gtLocalOpenings .= mempty
-- unless (isSharesIdx slotIdx) $ gtLocalShares .= mempty
