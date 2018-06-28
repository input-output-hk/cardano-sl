{-# LANGUAGE Rank2Types #-}

module Pos.Ssc.Logic.Local
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
import           Data.DList as DList (singleton)
import           Data.Functor.Contravariant (contramap)
import qualified Data.HashMap.Strict as HM
import           Formatting (int, sformat, (%))
import           Serokell.Util (magnify')

import           Pos.Binary.Class (biSize)
import           Pos.Binary.Ssc ()
import           Pos.Core (BlockVersionData (..), EpochIndex, HasGenesisData,
                     HasProtocolConstants, SlotId (..), StakeholderId,
                     VssCertificate, epochIndexL,
                     mkVssCertificatesMapSingleton)
import           Pos.Core.Ssc (InnerSharesMap, Opening, SignedCommitment,
                     SscPayload (..), mkCommitmentsMap)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB (MonadBlockDBRead, MonadDBRead,
                     MonadGState (gsAdoptedBVData))
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Lrc.Consumer.Ssc (getSscRichmen, tryGetSscRichmen)
import           Pos.Lrc.Context (HasLrcContext)
import           Pos.Lrc.Types (RichmenStakes)
import           Pos.Ssc.Base (isCommitmentIdx, isOpeningIdx, isSharesIdx)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Ssc.Error (SscVerifyError (..))
import           Pos.Ssc.Mem (MonadSscMem, SscLocalQuery, SscLocalUpdate,
                     askSscMem, sscRunGlobalQuery, sscRunLocalQuery,
                     sscRunLocalSTM)
import           Pos.Ssc.Toss (PureToss, SscTag (..), TossT,
                     evalPureTossWithLogger, evalTossT, execTossT,
                     hasCertificateToss, hasCommitmentToss, hasOpeningToss,
                     hasSharesToss, isGoodSlotForTag, normalizeToss,
                     pureTossWithEnvTrace, refreshToss, supplyPureTossEnv,
                     tmCertificates, tmCommitments, tmOpenings, tmShares,
                     verifyAndApplySscPayload)
import           Pos.Ssc.Types (SscGlobalState, SscLocalData (..), ldEpoch,
                     ldModifier, ldSize, sscGlobal)
import           Pos.Util.Trace (Trace, natTrace)
import           Pos.Util.Trace.Unstructured (LogItem, logWarning)
import           Pos.Util.Trace.Writer (writerTrace)

-- | Get local payload to be put into main block and for given
-- 'SlotId'. If payload for given 'SlotId' can't be constructed,
-- empty payload can be returned.
sscGetLocalPayload
    :: forall ctx m.
       (MonadIO m, MonadSscMem ctx m, HasProtocolConstants)
    => Trace m LogItem -> SlotId -> m SscPayload
sscGetLocalPayload logTrace si = sscRunLocalQuery (sscGetLocalPayloadQ si logTrace)

sscGetLocalPayloadQ
  :: (HasProtocolConstants)
  => SlotId -> SscLocalQuery SscPayload
sscGetLocalPayloadQ SlotId {..} logTrace = do
    expectedEpoch <- view ldEpoch
    let warningMsg = sformat warningFmt siEpoch expectedEpoch
    isExpected <-
        if expectedEpoch == siEpoch then pure True
        else False <$ lift (logWarning logTrace warningMsg)
    magnify' ldModifier $
        getPayload isExpected <*> getCertificates isExpected
  where
    warningFmt = "sscGetLocalPayloadQ: unexpected epoch ("%int%
                 ", stored one is "%int%")"
    getPayload True
        | isCommitmentIdx siSlot = CommitmentsPayload <$> view tmCommitments
        | isOpeningIdx siSlot = OpeningsPayload <$> view tmOpenings
        | isSharesIdx siSlot = SharesPayload <$> view tmShares
    getPayload _ = pure CertificatesPayload
    getCertificates isExpected
        | isExpected = view tmCertificates
        | otherwise = pure mempty

-- | Make 'SscLocalData' valid for given epoch, richmen and global state. of
-- best known chain. This function is assumed to be called after applying
-- block and before releasing lock on block application.
sscNormalize
    :: forall ctx m.
       ( MonadGState m
       , MonadBlockDBRead m
       , MonadSscMem ctx m
       , HasLrcContext ctx
       , MonadIO m
       , Rand.MonadRandom m
       )
    => Trace m LogItem
    -> ProtocolMagic -> m ()
sscNormalize logTrace pm = do
    tipEpoch <- view epochIndexL <$> getTipHeader
    richmenData <- getSscRichmen "sscNormalize" tipEpoch
    bvd <- gsAdoptedBVData
    globalVar <- sscGlobal <$> askSscMem
    --localVar <- sscLocal <$> askSscMem
    gs <- atomically $ readTVar globalVar
    seed <- Rand.drgNew

    sscRunLocalSTM logTrace $
        --syncingStateWith localVar  $
        executeMonadBaseRandom seed $
        sscNormalizeU pm (tipEpoch, richmenData) bvd gs
  where
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

sscNormalizeU
    :: (HasProtocolConstants, HasGenesisData)
    => ProtocolMagic
    -> (EpochIndex, RichmenStakes)
    -> BlockVersionData
    -> SscGlobalState
    -> SscLocalUpdate ()
sscNormalizeU pm (epoch, stake) bvd gs = do
    oldModifier <- lift $ use ldModifier
    let multiRichmen = HM.fromList [(epoch, stake)]
        logTrace = contramap DList.singleton writerTrace
    newModifier <-
        evalPureTossWithLogger gs logTrace $ supplyPureTossEnv (multiRichmen, bvd) $
        execTossT mempty $ normalizeToss
        (natTrace lift pureTossWithEnvTrace)
        pm epoch oldModifier
    ldModifier .= newModifier
    ldEpoch .= epoch
    ldSize .= biSize newModifier

----------------------------------------------------------------------------
---- Inv processing
----------------------------------------------------------------------------

-- | Check whether SSC data with given tag and public key can be added
-- to current local data.
sscIsDataUseful
    :: forall ctx m.
       ( MonadIO m
       , MonadSlots ctx m
       , MonadSscMem ctx m
       , Rand.MonadRandom m
       , HasGenesisData
       , HasProtocolConstants
       )
    => Trace m LogItem -> SscTag -> StakeholderId -> m Bool
sscIsDataUseful logTrace tag id =
    ifM
        (maybe False (isGoodSlotForTag tag . siSlot) <$> getCurrentSlot)
        (evalTossInMem $ sscIsDataUsefulDo tag)
        (pure False)
  where
    sscIsDataUsefulDo CommitmentMsg     = not <$> hasCommitmentToss id
    sscIsDataUsefulDo OpeningMsg        = not <$> hasOpeningToss id
    sscIsDataUsefulDo SharesMsg         = not <$> hasSharesToss id
    sscIsDataUsefulDo VssCertificateMsg = not <$> hasCertificateToss id
    evalTossInMem :: TossT PureToss a -> m a
    evalTossInMem action = do
        gs <- sscRunGlobalQuery ask
        ld <- sscRunLocalQuery ask
        let modifier = ld ^. ldModifier
        evalPureTossWithLogger gs logTrace $ evalTossT modifier action

----------------------------------------------------------------------------
---- Data processing
----------------------------------------------------------------------------

type SscDataProcessingMode ctx m =
    ( MonadIO m           -- STM at least
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
    => Trace m LogItem
    -> ProtocolMagic
    -> SignedCommitment
    -> m (Either SscVerifyError ())
sscProcessCommitment logTrace pm comm =
    sscProcessData logTrace pm CommitmentMsg
        $ CommitmentsPayload (mkCommitmentsMap [comm]) mempty

-- | Process 'Opening' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessOpening
    :: SscDataProcessingMode ctx m
    => Trace m LogItem
    -> ProtocolMagic
    -> StakeholderId
    -> Opening
    -> m (Either SscVerifyError ())
sscProcessOpening logTrace pm id opening =
    sscProcessData logTrace pm OpeningMsg $
    OpeningsPayload (HM.fromList [(id, opening)]) mempty

-- | Process 'InnerSharesMap' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessShares
    :: SscDataProcessingMode ctx m
    => Trace m LogItem
    -> ProtocolMagic
    -> StakeholderId
    -> InnerSharesMap
    -> m (Either SscVerifyError ())
sscProcessShares logTrace pm id shares =
    sscProcessData logTrace pm SharesMsg $
    SharesPayload (HM.fromList [(id, shares)]) mempty

-- | Process 'VssCertificate' received from network, checking it against
-- current state (global + local) and adding to local state if it's valid.
sscProcessCertificate
    :: SscDataProcessingMode ctx m
    => Trace m LogItem
    -> ProtocolMagic
    -> VssCertificate
    -> m (Either SscVerifyError ())
sscProcessCertificate logTrace pm cert =
    sscProcessData logTrace pm VssCertificateMsg $
    CertificatesPayload (mkVssCertificatesMapSingleton cert)

sscProcessData
    :: SscDataProcessingMode ctx m
    => Trace m LogItem
    -> ProtocolMagic
    -> SscTag
    -> SscPayload
    -> m (Either SscVerifyError ())
sscProcessData logTrace pm tag payload =
    runExceptT $ do
        getCurrentSlot >>= checkSlot
        ld <- sscRunLocalQuery ask
        bvd <- gsAdoptedBVData
        let epoch = ld ^. ldEpoch
        seed <- Rand.drgNew
        lift (tryGetSscRichmen epoch) >>= \case
            Nothing -> throwError $ TossUnknownRichmen epoch
            Just richmen -> do
                let logTrace' = contramap DList.singleton writerTrace
                gs <- sscRunGlobalQuery ask
                ExceptT $
                    sscRunLocalSTM logTrace $
                    executeMonadBaseRandom seed $
                    sscProcessDataDo logTrace' pm (epoch, richmen) bvd gs payload
  where
    checkSlot Nothing = throwError CurrentSlotUnknown
    checkSlot (Just si@SlotId {..})
        | isGoodSlotForTag tag siSlot = pass
        | CommitmentMsg <- tag = throwError $ NotCommitmentPhase si
        | OpeningMsg <- tag = throwError $ NotOpeningPhase si
        | SharesMsg <- tag = throwError $ NotSharesPhase si
        | otherwise = pass
    -- (... MonadPseudoRandom) a -> (... n) a
    executeMonadBaseRandom seed = hoist $ hoist (pure . fst . Rand.withDRG seed)

sscProcessDataDo
    :: (MonadState SscLocalData m, HasGenesisData
      , Rand.MonadRandom m, HasProtocolConstants)
    => Trace m LogItem
    -> ProtocolMagic
    -> (EpochIndex, RichmenStakes)
    -> BlockVersionData
    -> SscGlobalState
    -> SscPayload
    -> m (Either SscVerifyError ())
sscProcessDataDo logTrace pm richmenData bvd gs payload =
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
                   evalPureTossWithLogger gs (natTrace lift logTrace) .
                   supplyPureTossEnv (multiRichmen, bvd) .
                   execTossT mempty . refreshToss (natTrace lift pureTossWithEnvTrace) pm givenEpoch =<<
                   use  ldModifier
        newTM <-
            ExceptT $
            evalPureTossWithLogger gs logTrace $
            supplyPureTossEnv (multiRichmen, bvd) $
            runExceptT $
            execTossT oldTM $
            verifyAndApplySscPayload
                (natTrace (lift .lift) pureTossWithEnvTrace)
                pm
                (Left storedEpoch)
                payload
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
