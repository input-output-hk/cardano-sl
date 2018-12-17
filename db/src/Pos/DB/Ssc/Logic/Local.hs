{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

module Pos.DB.Ssc.Logic.Local
       (
         sscGetLocalPayload
       , sscNormalize

         -- * 'Inv|Req|Data' processing.
       , sscIsDataUseful
       , sscProcessCommitment
       , sscProcessOpening
       , sscProcessShares
       , sscProcessCertificate

         -- * Garbage collection
       , sscGarbageCollectLocalData
       ) where

import           Universum hiding (id)

import           Control.Lens ((+=), (.=))
import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Control.Monad.Morph (hoist)
import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import           Formatting (int, sformat, (%))
import           Serokell.Util (magnify')

import           Pos.Binary.Class (biSize)
import           Pos.Chain.Genesis as Genesis (Config, configBlkSecurityParam,
                     configBlockVersionData, configEpochSlots)
import           Pos.Chain.Lrc (RichmenStakes)
import           Pos.Chain.Ssc (HasSscConfiguration, InnerSharesMap,
                     MonadSscMem, Opening, PureToss, SignedCommitment,
                     SscGlobalState, SscLocalData (..), SscLocalQuery,
                     SscLocalUpdate, SscPayload (..), SscTag (..),
                     SscVerifyError (..), TossT, VssCertificate, askSscMem,
                     evalPureTossWithLogger, evalTossT, execTossT,
                     hasCertificateToss, hasCommitmentToss, hasOpeningToss,
                     hasSharesToss, isCommitmentIdx, isGoodSlotForTag,
                     isOpeningIdx, isSharesIdx, ldEpoch, ldModifier, ldSize,
                     mkCommitmentsMap, mkVssCertificatesMapSingleton,
                     normalizeToss, refreshToss, sscGlobal, sscLocal,
                     sscRunGlobalQuery, sscRunLocalQuery, sscRunLocalSTM,
                     supplyPureTossEnv, syncingStateWith, tmCertificates,
                     tmCommitments, tmOpenings, tmShares,
                     verifyAndApplySscPayload)
import           Pos.Chain.Update (BlockVersionData (..))
import           Pos.Core (BlockCount, EpochIndex, SlotId (..), StakeholderId,
                     epochIndexL, kEpochSlots)
import           Pos.Core.Slotting (MonadSlots (getCurrentSlot))
import           Pos.DB (MonadBlockDBRead, MonadDBRead,
                     MonadGState (gsAdoptedBVData))
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.DB.Lrc (HasLrcContext, getSscRichmen, tryGetSscRichmen)
import           Pos.Util.Wlog (WithLogger, launchNamedPureLog, logWarning)

-- | Get local payload to be put into main block and for given
-- 'SlotId'. If payload for given 'SlotId' can't be constructed,
-- empty payload can be returned.
sscGetLocalPayload
    :: forall ctx m.
       (MonadIO m, MonadSscMem ctx m, WithLogger m)
    => BlockCount -> SlotId -> m SscPayload
sscGetLocalPayload k = sscRunLocalQuery . sscGetLocalPayloadQ k

sscGetLocalPayloadQ
    :: BlockCount -> SlotId -> SscLocalQuery SscPayload
sscGetLocalPayloadQ k SlotId {..} = do
    expectedEpoch <- view ldEpoch
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if expectedEpoch == siEpoch then pure True
        else False <$ logWarning warningMsg
    magnify' ldModifier $
        getPayload isExpected <*> getCertificates isExpected
  where
    warningFmt = "sscGetLocalPayloadQ: unexpected epoch ("%int%
                 ", stored one is "%int%")"
    getPayload True
        | isCommitmentIdx k siSlot = CommitmentsPayload <$> view tmCommitments
        | isOpeningIdx k siSlot = OpeningsPayload <$> view tmOpenings
        | isSharesIdx k siSlot = SharesPayload <$> view tmShares
    getPayload _ = pure CertificatesPayload
    getCertificates isExpected
        | isExpected = view tmCertificates
        | otherwise = pure mempty

-- | Make 'SscLocalData' valid for given epoch, richmen and global state. of
-- best known chain. This function is assumed to be called after applying
-- block and before releasing lock on block application.
sscNormalize
    :: forall ctx m
     . ( MonadGState m
       , MonadBlockDBRead m
       , MonadSscMem ctx m
       , HasLrcContext ctx
       , WithLogger m
       , MonadIO m
       , Rand.MonadRandom m
       )
    => Genesis.Config
    -> m ()
sscNormalize genesisConfig = do
    tipEpoch <- view epochIndexL <$> getTipHeader
    richmenData <- getSscRichmen (configBlockVersionData genesisConfig)
                                 "sscNormalize"
                                 tipEpoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    localVar <- sscLocal <$> askSscMem
    gs <- readTVarIO globalVar
    seed <- Rand.drgNew

    launchNamedPureLog atomically $
        syncingStateWith localVar $
        executeMonadBaseRandom seed $
        sscNormalizeU genesisConfig (tipEpoch, richmenData) bvd gs
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

sscNormalizeU
    :: Genesis.Config
    -> (EpochIndex, RichmenStakes)
    -> BlockVersionData
    -> SscGlobalState
    -> SscLocalUpdate ()
sscNormalizeU genesisConfig (epoch, stake) bvd gs = do
    oldModifier <- use ldModifier
    let multiRichmen = HM.fromList [(epoch, stake)]
    newModifier <-
        evalPureTossWithLogger gs $ supplyPureTossEnv (multiRichmen, bvd) $
        execTossT mempty $ normalizeToss genesisConfig epoch oldModifier
    ldModifier .= newModifier
    ldEpoch .= epoch
    ldSize .= biSize newModifier

----------------------------------------------------------------------------
---- Inv processing
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to current local data.
sscIsDataUseful
    :: ( WithLogger m
       , MonadSlots ctx m
       , MonadSscMem ctx m
       , Rand.MonadRandom m
       )
    => BlockCount -> SscTag -> StakeholderId -> m Bool
sscIsDataUseful k tag id =
    ifM
        (maybe False (isGoodSlotForTag tag k . siSlot) <$> getCurrentSlot (kEpochSlots k))
        (evalTossInMem $ sscIsDataUsefulDo tag)
        (pure False)
  where
    sscIsDataUsefulDo CommitmentMsg     = not <$> hasCommitmentToss id
    sscIsDataUsefulDo OpeningMsg        = not <$> hasOpeningToss id
    sscIsDataUsefulDo SharesMsg         = not <$> hasSharesToss id
    sscIsDataUsefulDo VssCertificateMsg = not <$> hasCertificateToss id
    evalTossInMem
        :: ( WithLogger m
           , MonadIO m
           , MonadSscMem ctx m
           , Rand.MonadRandom m
           )
        => TossT PureToss a -> m a
    evalTossInMem action = do
        gs <- sscRunGlobalQuery ask
        ld <- sscRunLocalQuery ask
        let modifier = ld ^. ldModifier
        evalPureTossWithLogger gs $ evalTossT modifier action

----------------------------------------------------------------------------
---- Data processing
----------------------------------------------------------------------------

type SscDataProcessingMode ctx m =
    ( WithLogger m
    , MonadIO m           -- STM at least
    , Rand.MonadRandom m  -- for crypto
    , MonadDBRead m       -- to get richmen
    , MonadGState m       -- to get block size limit
    , MonadSlots ctx m
    , MonadSscMem ctx m
    , HasSscConfiguration
    )

-- | Process 'SignedCommitment' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCommitment
    :: SscDataProcessingMode ctx m
    => Genesis.Config
    -> SignedCommitment
    -> m (Either SscVerifyError ())
sscProcessCommitment genesisConfig comm =
    sscProcessData genesisConfig CommitmentMsg
        $ CommitmentsPayload (mkCommitmentsMap [comm]) mempty

-- | Process 'Opening' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessOpening
    :: SscDataProcessingMode ctx m
    => Genesis.Config
    -> StakeholderId
    -> Opening
    -> m (Either SscVerifyError ())
sscProcessOpening genesisConfig id opening = sscProcessData genesisConfig OpeningMsg
    $ OpeningsPayload (HM.fromList [(id, opening)]) mempty

-- | Process 'InnerSharesMap' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessShares
    :: SscDataProcessingMode ctx m
    => Genesis.Config
    -> StakeholderId
    -> InnerSharesMap
    -> m (Either SscVerifyError ())
sscProcessShares genesisConfig id shares =
    sscProcessData genesisConfig SharesMsg $ SharesPayload (HM.fromList [(id, shares)]) mempty

-- | Process 'VssCertificate' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCertificate
    :: SscDataProcessingMode ctx m
    => Genesis.Config
    -> VssCertificate
    -> m (Either SscVerifyError ())
sscProcessCertificate genesisConfig cert = sscProcessData genesisConfig VssCertificateMsg
    $ CertificatesPayload (mkVssCertificatesMapSingleton cert)

sscProcessData
    :: SscDataProcessingMode ctx m
    => Genesis.Config
    -> SscTag
    -> SscPayload
    -> m (Either SscVerifyError ())
sscProcessData genesisConfig tag payload =
    runExceptT $ do
        getCurrentSlot (configEpochSlots genesisConfig) >>= checkSlot
        ld <- sscRunLocalQuery ask
        bvd <- gsAdoptedBVData
        let epoch = ld ^. ldEpoch
        seed <- Rand.drgNew
        lift (tryGetSscRichmen (configBlockVersionData genesisConfig) epoch) >>= \case
            Nothing -> throwError $ TossUnknownRichmen epoch
            Just richmen -> do
                gs <- sscRunGlobalQuery ask
                ExceptT $
                    sscRunLocalSTM $
                    executeMonadBaseRandom seed $
                    sscProcessDataDo genesisConfig (epoch, richmen) bvd gs payload
  where
    checkSlot Nothing = throwError CurrentSlotUnknown
    checkSlot (Just si@SlotId {..})
        | isGoodSlotForTag tag (configBlkSecurityParam genesisConfig) siSlot = pass
        | CommitmentMsg <- tag = throwError $ NotCommitmentPhase si
        | OpeningMsg <- tag = throwError $ NotOpeningPhase si
        | SharesMsg <- tag = throwError $ NotSharesPhase si
        | otherwise = pass
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

sscProcessDataDo
    :: (MonadState SscLocalData m, WithLogger m, Rand.MonadRandom m)
    => Genesis.Config
    -> (EpochIndex, RichmenStakes)
    -> BlockVersionData
    -> SscGlobalState
    -> SscPayload
    -> m (Either SscVerifyError ())
sscProcessDataDo genesisConfig richmenData bvd gs payload =
    runExceptT $ do
        storedEpoch <- use ldEpoch
        let givenEpoch = fst richmenData
        let multiRichmen = HM.fromList [richmenData]
        unless (storedEpoch == givenEpoch) $
            throwError $ DifferentEpoches storedEpoch givenEpoch
        -- TODO: This is a rather arbitrary limit, we should revisit it (see CSL-1664)
        let maxMemPoolSize = bvdMaxBlockSize bvd * 2
        curSize <- use ldSize
        let exhausted = curSize >= maxMemPoolSize
        -- If our mempool is exhausted we drop some data from it.
        oldTM <-
            if | not exhausted -> use ldModifier
               | otherwise ->
                   evalPureTossWithLogger gs .
                   supplyPureTossEnv (multiRichmen, bvd) .
                   execTossT mempty . refreshToss genesisConfig givenEpoch =<<
                   use ldModifier
        newTM <-
            ExceptT $
            evalPureTossWithLogger gs $
            supplyPureTossEnv (multiRichmen, bvd) $
            runExceptT $
            execTossT oldTM $
            verifyAndApplySscPayload genesisConfig (Left storedEpoch) payload
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
sscGarbageCollectLocalData
    :: MonadSscMem ctx m
    => SlotId -> m ()
sscGarbageCollectLocalData _ = pass
-- unless (isCommitmentIdx slotIdx) $ sscLocalCommitments .= mempty
-- unless (isOpeningIdx slotIdx) $ sscLocalOpenings .= mempty
-- unless (isSharesIdx slotIdx) $ sscLocalShares .= mempty
