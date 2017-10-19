-- | This module defines an instance of 'MonadRecoveryInfo'.

module Pos.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except   (runExceptT, throwError)

import           Pos.Context.Context    (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Core               (HasCoreConfiguration, epochOrSlotG,
                                         epochOrSlotToSlot, flattenSlotId)
import           Pos.DB.Block           (MonadBlockDB)
import           Pos.DB.DB              (getTipHeader)
import           Pos.Recovery.Info      (MonadRecoveryInfo (..), SyncStatus (..))
import           Pos.Slotting           (MonadSlots (getCurrentSlot))
import           Pos.Util.Util          (HasLens (..))

instance ( Monad m
         , MonadIO m
         , MonadBlockDB m
         , MonadSlots ctx m
         , MonadReader ctx m
         , HasLens RecoveryHeaderTag ctx RecoveryHeader
         , HasCoreConfiguration
         ) =>
         MonadRecoveryInfo m where
    getSyncStatus lagBehindParam =
        fmap convertRes . runExceptT $ do
            recoveryIsInProgress >>= \case
                False -> pass
                True -> throwError SSDoingRecovery
            curSlot <- note SSUnknownSlot =<< getCurrentSlot
            tipHeader <- getTipHeader
            let tipSlot = epochOrSlotToSlot (tipHeader ^. epochOrSlotG)
            unless (tipSlot <= curSlot) $
                throwError
                    SSInFuture
                    {sslbCurrentSlot = curSlot, sslbTipSlot = tipSlot}
            let slotDiff = flattenSlotId curSlot - flattenSlotId tipSlot
            unless (slotDiff < fromIntegral lagBehindParam) $
                throwError
                    SSLagBehind
                    {sslbCurrentSlot = curSlot, sslbTipSlot = tipSlot}
      where
        recoveryIsInProgress = do
            var <- view (lensOf @RecoveryHeaderTag)
            isJust <$> atomically (STM.tryReadTMVar var)
        convertRes :: Either SyncStatus () -> SyncStatus
        convertRes (Left ss)  = ss
        convertRes (Right ()) = SSKindaSynced
