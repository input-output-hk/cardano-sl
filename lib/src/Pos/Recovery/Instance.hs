{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An instance of 'MonadRecoveryInfo'.

module Pos.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except (runExceptT, throwError)

import           Pos.Block.BHelpers ()
import           Pos.Core (HasProtocolConstants, epochOrSlotG,
                     epochOrSlotToSlot, flattenSlotId)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Infra.Recovery.Info (CurrentSlot (..),
                     MonadRecoveryInfo (..), SyncStatus (..), TipSlot (..))
import           Pos.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Pos.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Util.Util (HasLens (..))

instance ( Monad m
         , MonadIO m
         , MonadDBRead m
         , MonadSlots ctx m
         , MonadReader ctx m
         , HasLens RecoveryHeaderTag ctx RecoveryHeader
         , HasProtocolConstants
         ) =>
         MonadRecoveryInfo m where
    getSyncStatus lagBehindParam =
        fmap convertRes . runExceptT $ do
            recoveryIsInProgress >>= \case
                False -> pass
                True -> throwError SSDoingRecovery
            curSlotId <- note SSUnknownSlot =<< getCurrentSlot
            tipHeader <- lift DB.getTipHeader
            let curSlot = CurrentSlot curSlotId
            let tipSlot@(TipSlot tipSlotId) = TipSlot $
                    epochOrSlotToSlot (tipHeader ^. epochOrSlotG)
            unless (tipSlotId <= curSlotId) $
                throwError $
                    SSInFuture tipSlot curSlot
            let slotDiff = flattenSlotId curSlotId - flattenSlotId tipSlotId
            unless (slotDiff < fromIntegral lagBehindParam) $
                throwError $
                    SSLagBehind tipSlot curSlot
      where
        recoveryIsInProgress = do
            var <- view (lensOf @RecoveryHeaderTag)
            isJust <$> atomically (STM.tryReadTMVar var)
        convertRes :: Either SyncStatus () -> SyncStatus
        convertRes (Left ss)  = ss
        convertRes (Right ()) = SSKindaSynced
