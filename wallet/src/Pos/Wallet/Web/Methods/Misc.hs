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
       , dumpState
       , WalletStateSnapshot (..)
       ) where

import           Universum

import           Data.Aeson                   (encode)
import           Data.Aeson.TH                (defaultOptions, deriveJSON)
import qualified Data.Text.Buildable
import           Mockable                     (MonadMockable)
import           Pos.Core                     (SoftwareVersion (..))
import           Pos.Update.Configuration     (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util                     (maybeThrow)
import           Servant.API.ContentTypes     (MimeRender (..), OctetStream)

import           Pos.Client.KeyStorage        (MonadKeys, deleteSecretKey, getSecretKeys)
import           Pos.NtpCheck                 (NtpCheckMonad, NtpStatus (..),
                                               mkNtpStatusVar)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.Storage     ()
import           Pos.Wallet.WalletMode        (MonadBlockchainInfo, MonadUpdates,
                                               applyLastUpdate, connectedPeers,
                                               localChainDifficulty,
                                               networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes   (Addr, CId, CProfile (..), CUpdateInfo (..),
                                               SyncProgress (..), cIdToAddress)
import           Pos.Wallet.Web.Error         (WalletError (..))
import           Pos.Wallet.Web.State         (MonadWalletDB, MonadWalletDBRead (..),
                                               getNextUpdate, getProfile,
                                               removeNextUpdate, setProfile, testReset,
                                               unDBWalletStorage)
import           Pos.Wallet.Web.State.Storage (WalletStorage)

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: MonadWalletDBRead m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletDB ctx m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

isValidAddress :: Monad m => CId Addr -> m Bool
isValidAddress = pure . isRight . cIdToAddress

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
    diff <$> (mkNtpStatusVar >>= readMVar)
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

----------------------------------------------------------------------------
-- Print wallet state
----------------------------------------------------------------------------

data WalletStateSnapshot = WalletStateSnapshot
    { wssWalletStorage :: WalletStorage
    } deriving (Generic)

deriveJSON defaultOptions ''WalletStateSnapshot

instance MimeRender OctetStream WalletStateSnapshot where
    mimeRender _ = encode

instance Buildable WalletStateSnapshot where
    build _ = "<wallet-state-snapshot>"

dumpState :: MonadWalletDBRead m => m WalletStateSnapshot
dumpState = WalletStateSnapshot . unDBWalletStorage <$> getDBWalletStorage
