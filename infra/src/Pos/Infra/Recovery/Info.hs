-- | The main goal of this module is to encapsulate recovery mechanism
-- and provide helpers related to it.

module Pos.Infra.Recovery.Info
       ( CurrentSlot (..)
       , TipSlot (..)
       , SyncStatus (..)
       , MonadRecoveryInfo
       , getSyncStatus
       , recoveryInProgress
       , getSyncStatusK
       , recoveryCommGuard
       , needTriggerRecovery
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except (runExceptT, throwError)
import           Formatting (bprint, build, sformat, stext, (%))
import qualified Formatting.Buildable
import           System.Wlog (WithLogger, logDebug)

import           Pos.Core (SlotCount, SlotId, epochOrSlotG, epochOrSlotToSlot,
                     flattenSlotId, slotIdF, slotSecurityParam)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Infra.Slotting.Class (MonadSlots (getCurrentSlot))
import           Pos.Util.Util (HasLens (..))

newtype TipSlot = TipSlot SlotId

newtype CurrentSlot = CurrentSlot SlotId

-- | An algebraic data type which represents how well we are
-- synchronized with the network.
data SyncStatus
    = SSDoingRecovery
    -- ^ We are doing recovery right now, so it's better to wait until
    -- we finish.
    | SSUnknownSlot
    -- ^ We don't know current slot, so we are definitely not
    -- synchronized well enough.
    | SSLagBehind !TipSlot
                  --  Our tip's slot (if our tip is genesis, we use 0
                  -- as local slot).
                  !CurrentSlot
                  --  We know current slot, but our tip's slot lags behind
                  -- current slot too much.
    | SSInFuture !TipSlot
                 !CurrentSlot
                 --  We know current slot and our tip's slot is greater than
                 -- the current one. Most likely we are misconfigured or we are
                 -- cheating somehow (e. g. creating blocks using block-gen).
    | SSKindaSynced
    -- ^ We are kinda synchronized, i. e. all previously described
    -- statuses are not about us.

instance Buildable SyncStatus where
    build =
        \case
            SSDoingRecovery -> "we're doing recovery"
            SSUnknownSlot -> "we don't know current slot"
            SSLagBehind (TipSlot sslbTipSlot) (CurrentSlot sslbCurrentSlot) ->
                bprint
                    ("we lag behind too much, our tip's slot is: " %slotIdF %
                     ", but current slot is " %slotIdF)
                    sslbTipSlot
                    sslbCurrentSlot
            SSInFuture (TipSlot sslbTipSlot) (CurrentSlot sslbCurrentSlot) ->
                bprint
                    ("we invented a time machine, our tip's slot is: " %slotIdF %
                     " and it's greater than current slot: " %slotIdF)
                    sslbTipSlot
                    sslbCurrentSlot
            SSKindaSynced -> "we are moderately synchronized"

type MonadRecoveryInfo ctx m =
    ( Monad m
    , MonadIO m
    , MonadDBRead m
    , MonadSlots ctx m
    , MonadReader ctx m
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    )

-- | Returns our synchronization status. The argument determines
-- how much we should lag behind for 'SSLagBehind' status to take
-- place. See 'SyncStatus' for details.
-- Implementation must check conditions in the same order as they
-- are enumerated in 'SyncStatus'.
getSyncStatus :: MonadRecoveryInfo ctx m => SlotCount -> m SyncStatus
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

-- | Returns if our 'SyncStatus' is 'SSDoingRecovery' (which is
-- equivalent to “we're doing recovery”).
recoveryInProgress :: MonadRecoveryInfo ctx m => m Bool
recoveryInProgress =
    getSyncStatus 0 {- 0 doesn't matter -} <&> \case
        SSDoingRecovery -> True
        _ -> False

-- | Get sync status using K as lagBehind param.
getSyncStatusK
    :: MonadRecoveryInfo ctx m
    => m SyncStatus
getSyncStatusK = getSyncStatus lagBehindParam
  where
    -- It's actually questionable which value to use here. The less it
    -- is, the stricter is the condition to do some
    -- work. 'slotSecurityParam' is reasonable, but maybe we should use
    -- something smaller.
    lagBehindParam :: SlotCount
    lagBehindParam = slotSecurityParam

-- | This is a helper function which runs given action only if we are
-- kinda synchronized with the network.  It is useful for workers
-- which shouldn't do anything while we are not synchronized.
recoveryCommGuard
    :: (MonadRecoveryInfo ctx m, WithLogger m)
    => Text -> m () -> m ()
recoveryCommGuard actionName action =
    getSyncStatusK >>= \case
        SSKindaSynced -> action
        status ->
            logDebug $
            sformat ("recoveryCommGuard: we are skipping action '"%stext%
                     "', because "%build) actionName status

-- | This function checks that last known block is more than K slots
-- away from the current slot, or current slot isn't known. It also
-- returns False when we're actually doing recovery. So basically it
-- returns true if we actually need to ask for tips right now.
needTriggerRecovery :: SyncStatus -> Bool
needTriggerRecovery = \case
    SSKindaSynced   -> False
    SSDoingRecovery -> False
    SSInFuture{}    -> False
    _               -> True
