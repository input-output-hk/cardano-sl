-- | Main Toss logic.

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Ssc.GodTossing.Toss.Logic
       ( verifyAndApplyGtPayload
       , applyGenesisBlock
       , rollbackGT
       , normalizeToss
       ) where

import           Control.Monad.Except            (MonadError (throwError), runExceptT)
import qualified Data.HashMap.Strict             as HM
import           Formatting                      (sformat, (%))
import           Serokell.Util.Text              (listJson)
import           System.Wlog                     (logError)
import           Universum

import           Pos.Constants                   (slotSecurityParam)
import           Pos.Ssc.GodTossing.Core         (CommitmentsMap (..), GtPayload (..),
                                                  getCommitmentsMap,
                                                  mkCommitmentsMapUnsafe, _gpCertificates)
import           Pos.Ssc.GodTossing.Functions    (verifyGtPayload)
import           Pos.Ssc.GodTossing.Toss.Base    (checkPayload)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Toss.Types   (TossModifier (..))
import           Pos.Ssc.GodTossing.Type         ()
import           Pos.Types                       (EpochIndex, EpochOrSlot (..),
                                                  LocalSlotIndex (getSlotIndex),
                                                  MainBlockHeader, SlotId (siSlot),
                                                  epochIndexL, epochOrSlot,
                                                  getEpochOrSlot)
import           Pos.Util                        (NewestFirst (..))

-- | Verify 'GtPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- TossVerFailure is thrown using 'MonadError' type class.
verifyAndApplyGtPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => Either EpochIndex (MainBlockHeader ssc) -> GtPayload -> m ()
verifyAndApplyGtPayload eoh payload = do
    verifyGtPayload eoh payload  -- not necessary for blocks, but just in case
    let blockCerts = _gpCertificates payload
    let curEpoch = either identity (^. epochIndexL) eoh
    checkPayload curEpoch payload

    -- Apply
    case eoh of
        Left _       -> pass
        Right header -> do
            let eos = getEpochOrSlot header
            setEpochOrSlot eos
            let slot = epochOrSlot (const 0) (getSlotIndex . siSlot) eos
            -- We can freely clear shares after 'slotSecurityParam'
            -- because it's guaranteed that rollback on more than 'slotSecurityParam'
            -- can't happen
            when (slot >= slotSecurityParam && slot < 2 * slotSecurityParam) resetShares
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

-- | Rollback application of 'GtPayload's in 'Toss'. First argument is
-- 'EpochOrSlot' of oldest block which is subject to rollback.
rollbackGT :: MonadToss m => EpochOrSlot -> NewestFirst [] GtPayload -> m ()
rollbackGT oldestEOS (NewestFirst payloads)
    | oldestEOS == toEnum 0 = do
        logError "rollbackGT: most genesis block is passed to rollback"
        setEpochOrSlot oldestEOS
        resetCO
        resetShares
    | otherwise = do
        setEpochOrSlot (pred oldestEOS)
        mapM_ rollbackGTDo payloads
  where
    rollbackGTDo (CommitmentsPayload comms _) =
        mapM_ delCommitment $ HM.keys $ getCommitmentsMap comms
    rollbackGTDo (OpeningsPayload opens _) = mapM_ delOpening $ HM.keys opens
    rollbackGTDo (SharesPayload shares _) = mapM_ delShares $ HM.keys shares
    rollbackGTDo (CertificatesPayload _) = pass

-- | Apply as much data from given 'TossModifier' as possible.
normalizeToss
    :: forall m . MonadToss m
    => EpochIndex -> TossModifier -> m ()
normalizeToss epoch TossModifier{..} = do
    putsUseful $ map (flip CommitmentsPayload mempty . mkCommitmentsMapUnsafe . one) $
                 HM.toList $
                 getCommitmentsMap _tmCommitments
    putsUseful $ map (flip OpeningsPayload mempty . one) $ HM.toList _tmOpenings
    putsUseful $ map (flip SharesPayload mempty . one) $ HM.toList _tmShares
    putsUseful $ map (CertificatesPayload . one) $ HM.toList _tmCertificates
  where
    putsUseful :: [GtPayload] -> m ()
    putsUseful entries = do
        let verifyAndApply = runExceptT . verifyAndApplyGtPayload (Left epoch)
        mapM_ verifyAndApply entries
