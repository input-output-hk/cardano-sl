{-# LANGUAGE RecordWildCards #-}

-- | Main Toss logic.

module Pos.Chain.Ssc.Toss.Logic
       ( verifyAndApplySscPayload
       , applyGenesisBlock
       , rollbackSsc
       , normalizeToss
       , refreshToss
       ) where

import           Universum hiding (id)

import           Control.Lens (at)
import           Control.Monad.Except (MonadError, runExceptT, throwError)
import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM
import           System.Wlog (logError)

import           Pos.Chain.Block.Union (IsMainHeader, headerSlotL)
import           Pos.Chain.Ssc.Error (SscVerifyError (..))
import           Pos.Chain.Ssc.Functions (verifySscPayload)
import           Pos.Chain.Ssc.Toss.Base (checkPayload)
import           Pos.Chain.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..))
import           Pos.Chain.Ssc.Toss.Types (TossModifier (..))
import           Pos.Core (EpochIndex, EpochOrSlot (..), HasProtocolConstants,
                     LocalSlotIndex, SlotCount, SlotId (siSlot), StakeholderId,
                     epochIndexL, epochOrSlot, getEpochOrSlot, mkCoin,
                     slotSecurityParam)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Ssc (CommitmentsMap (..), InnerSharesMap, Opening,
                     SignedCommitment, SscPayload (..), VssCertificate,
                     checkSscPayload, getCommitmentsMap, getVssCertificatesMap,
                     mkCommitmentsMapUnsafe, mkVssCertificatesMapSingleton,
                     spVss)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Some (Some)
import           Pos.Util.Util (sortWithMDesc)

-- | Verify 'SscPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- SscVerifyError is thrown using 'MonadError' type class.
verifyAndApplySscPayload
    :: ( HasProtocolConstants
       , MonadError SscVerifyError m
       , MonadRandom m
       , MonadToss m
       , MonadTossEnv m
       )
    => ProtocolMagic
    -> Either EpochIndex (Some IsMainHeader)
    -> SscPayload
    -> m ()
verifyAndApplySscPayload pm eoh payload = do
    -- Check the payload for internal consistency.
    either (throwError . SscInvalidPayload) pure (checkSscPayload pm payload)
    -- We can't trust payload from mempool, so we must call
    -- @verifySscPayload@.
    whenLeft eoh $ const $ verifySscPayload pm eoh payload
    -- We perform @verifySscPayload@ for block when we construct it
    -- (in the 'recreateGenericBlock').  So this check is just in case.
    -- NOTE: in block verification `verifySscPayload` on `MainBlock` is run
    -- by `Pos.Block.BHelper.verifyMainBlock`
    inAssertMode $
        whenRight eoh $ const $ verifySscPayload pm eoh payload
    let blockCerts = spVss payload
    let curEpoch = either identity (^. epochIndexL) eoh
    checkPayload curEpoch payload

    -- Apply
    case eoh of
        Left _       -> pass
        Right header -> do
            let eos = EpochOrSlot $ Right $ header ^. headerSlotL
            setEpochOrSlot eos
            -- We can freely clear shares after 'slotSecurityParam' because
            -- it's guaranteed that rollback on more than 'slotSecurityParam'
            -- can't happen
            let indexToCount :: LocalSlotIndex -> SlotCount
                indexToCount = fromIntegral . fromEnum
            let slot = epochOrSlot (const 0) (indexToCount . siSlot) eos
            when (slotSecurityParam <= slot && slot < 2 * slotSecurityParam) $
                resetShares
    mapM_ putCertificate blockCerts
    case payload of
        CommitmentsPayload  comms  _ ->
            mapM_ putCommitment $ toList $ getCommitmentsMap comms
        OpeningsPayload     opens  _ ->
            mapM_ (uncurry putOpening) $ HM.toList opens
        SharesPayload       shares _ ->
            mapM_ (uncurry putShares) $ HM.toList shares
        CertificatesPayload        _ ->
            pass

-- | Apply genesis block for given epoch to 'Toss' state.
applyGenesisBlock :: MonadToss m => EpochIndex -> m ()
applyGenesisBlock epoch = do
    setEpochOrSlot $ getEpochOrSlot epoch
    -- We don't clear shares on genesis block because
    -- there aren't 'slotSecurityParam' slots after shares phase,
    -- so we won't have shares after rollback
    -- We store shares until 'slotSecurityParam' slots of next epoch passed
    -- and clear their after that
    resetCO

-- | Rollback application of 'SscPayload's in 'Toss'. First argument is
-- 'EpochOrSlot' of oldest block which is subject to rollback.
rollbackSsc :: (MonadToss m, HasProtocolConstants) =>
    EpochOrSlot
    -> NewestFirst [] SscPayload
    -> m ()
rollbackSsc oldestEOS (NewestFirst payloads)
    | oldestEOS == toEnum 0 = do
        logError "rollbackSsc: most genesis block is passed to rollback"
        setEpochOrSlot oldestEOS
        resetCO
        resetShares
    | otherwise = do
        setEpochOrSlot (pred oldestEOS)
        mapM_ rollbackSscDo payloads
  where
    rollbackSscDo (CommitmentsPayload comms _) =
        mapM_ delCommitment $ HM.keys $ getCommitmentsMap comms
    rollbackSscDo (OpeningsPayload opens _) = mapM_ delOpening $ HM.keys opens
    rollbackSscDo (SharesPayload shares _) = mapM_ delShares $ HM.keys shares
    rollbackSscDo (CertificatesPayload _) = pass

-- | Apply as much data from given 'TossModifier' as possible.
normalizeToss
    :: (MonadToss m, MonadTossEnv m, MonadRandom m, HasProtocolConstants)
    => ProtocolMagic -> EpochIndex -> TossModifier -> m ()
normalizeToss pm epoch TossModifier {..} =
    normalizeTossDo
        pm
        epoch
        ( HM.toList (getCommitmentsMap _tmCommitments)
        , HM.toList _tmOpenings
        , HM.toList _tmShares
        , HM.toList (getVssCertificatesMap _tmCertificates))

-- | Apply the most valuable from given 'TossModifier' and drop the
-- rest. This function can be used if mempool is exhausted.
refreshToss
    :: (MonadToss m, MonadTossEnv m, MonadRandom m, HasProtocolConstants)
    => ProtocolMagic -> EpochIndex -> TossModifier -> m ()
refreshToss pm epoch TossModifier {..} = do
    comms <-
        takeMostValuable epoch (HM.toList (getCommitmentsMap _tmCommitments))
    opens <- takeMostValuable epoch (HM.toList _tmOpenings)
    shares <- takeMostValuable epoch (HM.toList _tmShares)
    certs <- takeMostValuable epoch (HM.toList (getVssCertificatesMap _tmCertificates))
    normalizeTossDo pm epoch (comms, opens, shares, certs)

takeMostValuable
    :: (MonadToss m, MonadTossEnv m)
    => EpochIndex
    -> [(StakeholderId, x)]
    -> m [(StakeholderId, x)]
takeMostValuable epoch items = take toTake <$> sortWithMDesc resolver items
  where
    toTake = 2 * length items `div` 3
    resolver (id, _) =
        fromMaybe (mkCoin 0) . (view (at id)) . fromMaybe mempty <$>
        getRichmen epoch

type TossModifierLists
     = ( [(StakeholderId, SignedCommitment)]
       , [(StakeholderId, Opening)]
       , [(StakeholderId, InnerSharesMap)]
       , [(StakeholderId, VssCertificate)])

normalizeTossDo
    :: forall m.
       (MonadToss m, MonadTossEnv m, MonadRandom m, HasProtocolConstants)
    => ProtocolMagic -> EpochIndex -> TossModifierLists -> m ()
normalizeTossDo pm epoch (comms, opens, shares, certs) = do
    putsUseful $
        map (flip CommitmentsPayload mempty . mkCommitmentsMapUnsafe . one) $
        comms
    putsUseful $ map (flip OpeningsPayload mempty . one) opens
    putsUseful $ map (flip SharesPayload mempty . one) shares
    putsUseful $ map (CertificatesPayload . mkVssCertificatesMapSingleton . snd) certs
  where
    putsUseful :: [SscPayload] -> m ()
    putsUseful entries = do
        let verifyAndApply = runExceptT . verifyAndApplySscPayload pm (Left epoch)
        mapM_ verifyAndApply entries
