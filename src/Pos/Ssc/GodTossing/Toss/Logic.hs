-- | Main Toss logic.

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Ssc.GodTossing.Toss.Logic
       ( verifyAndApplyGtPayload
       , applyGenesisBlock
       , rollbackGT
       , normalizeToss
       ) where

import           Control.Monad.Except            (MonadError, runExceptT)
import qualified Data.HashMap.Strict             as HM
import           Formatting                      (sformat, (%))
import           Serokell.Util.Text              (listJson)
import           System.Wlog                     (logDebug, logError)
import           Universum

import           Control.Monad.Except            (MonadError (throwError))
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
                                                  MainBlockHeader, epochIndexL,
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
    stableCerts <- getStableCertificates curEpoch
    richmenSet <- maybe (throwError $ NoRichmen curEpoch) pure =<< getRichmen curEpoch
    logDebug $ sformat (" Stable certificates: "%listJson
                        %", richmen: "%listJson)
                stableCerts richmenSet
    checkPayload curEpoch payload

    -- Apply
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
    resetCOS

-- | Rollback application of 'GtPayload's in 'Toss'. First argument is
-- 'EpochOrSlot' of oldest block which is subject to rollback.
rollbackGT :: MonadToss m => EpochOrSlot -> NewestFirst [] GtPayload -> m ()
rollbackGT oldestEOS (NewestFirst payloads)
    | oldestEOS == toEnum 0 = do
        logError "rollbackGT: most genesis block is passed to rollback"
        setEpochOrSlot oldestEOS
        resetCOS
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
    putsUseful $ map (flip CommitmentsPayload mempty . mkCommitmentsMapUnsafe . hm) $
                 HM.toList $
                 getCommitmentsMap _tmCommitments
    putsUseful $ map (flip OpeningsPayload mempty . hm) $ HM.toList _tmOpenings
    putsUseful $ map (flip SharesPayload mempty . hm) $ HM.toList _tmShares
    putsUseful $ map (CertificatesPayload . hm) $ HM.toList _tmCertificates
  where
    hm = uncurry HM.singleton

    putsUseful :: [GtPayload] -> m ()
    putsUseful entries = do
        let verifyAndApply = runExceptT . verifyAndApplyGtPayload (Left epoch)
        mapM_ verifyAndApply entries
