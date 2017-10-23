{-# LANGUAGE TypeFamilies #-}

-- | Various small endpoints

module Pos.Wallet.Web.Methods.Misc
       ( getUserProfile
       , updateUserProfile

       , isValidAddress

       , nextUpdate
       , postponeUpdate
       , applyUpdate

       , syncProgress
       , localTimeDifference

       , testResetAll
       ) where

import           Universum

import           Mockable                   (MonadMockable)

import           Pos.Client.KeyStorage      (MonadKeys, deleteSecretKey, getSecretKeys)
import           Pos.Core                   (SoftwareVersion (..), decodeTextAddress)
import           Pos.NtpCheck               (NtpCheckMonad, NtpStatus (..),
                                             mkNtpStatusVar)
import           Pos.Update.Configuration   (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util                   (maybeThrow)

import           Pos.Wallet.WalletMode      (MonadBlockchainInfo (..), MonadUpdates (..))
import           Pos.Wallet.Web.ClientTypes (CProfile (..), CUpdateInfo (..),
                                             SyncProgress (..))
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (MonadWalletDB, MonadWalletDBRead,
                                             getNextUpdate, getProfile, removeNextUpdate,
                                             setProfile, testReset)


----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: MonadWalletDBRead ctx m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletDB ctx m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: Monad m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Get last update info
nextUpdate
    :: (MonadThrow m, MonadWalletDB ctx m, HasUpdateConfiguration)
    => m CUpdateInfo
nextUpdate = do
    updateInfo <- getNextUpdate >>= maybeThrow noUpdates
    if isUpdateActual (cuiSoftwareVersion updateInfo)
        then pure updateInfo
        else removeNextUpdate >> nextUpdate
  where
    isUpdateActual :: SoftwareVersion -> Bool
    isUpdateActual ver = svAppName ver == svAppName curSoftwareVersion
        && svNumber ver > svNumber curSoftwareVersion
    noUpdates = RequestError "No updates available"

-- | Postpone next update after restart
postponeUpdate :: MonadWalletDB ctx m => m ()
postponeUpdate = removeNextUpdate

-- | Delete next update info and restart immediately
applyUpdate :: (MonadWalletDB ctx m, MonadUpdates m) => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

syncProgress :: MonadBlockchainInfo m => m SyncProgress
syncProgress =
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

----------------------------------------------------------------------------
-- NTP (Network Time Protocol) based time difference
----------------------------------------------------------------------------

localTimeDifference :: (NtpCheckMonad m, MonadMockable m) => m Word
localTimeDifference =
    mkNtpStatusVar >>= readMVar >>= pure . diff
  where
    diff :: NtpStatus -> Word
    diff = \case
        NtpSyncOk -> 0
        -- ^ `NtpSyncOk` considered already a `timeDifferenceWarnThreshold`
        -- so that we can return 0 here to show there is no difference in time
        NtpDesync diff' -> fromIntegral diff'

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll :: (MonadWalletDB ctx m, MonadKeys m) => m ()
testResetAll = deleteAllKeys >> testReset
  where
    deleteAllKeys :: MonadKeys m => m ()
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0
