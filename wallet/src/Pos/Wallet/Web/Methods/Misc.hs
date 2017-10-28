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

       , testResetAll
       ) where

import           Universum

import           Pos.Aeson.ClientTypes      ()
import           Pos.Core                   (SoftwareVersion (..))
import           Pos.Update.Configuration   (curSoftwareVersion)
import           Pos.Util                   (maybeThrow)

import           Pos.Wallet.KeyStorage      (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode      (applyLastUpdate, connectedPeers,
                                             localChainDifficulty, networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (Addr, CId, CProfile (..), CUpdateInfo (..),
                                             SyncProgress (..), cIdToAddress)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.Mode        (MonadWalletWebMode)
import           Pos.Wallet.Web.State       (getNextUpdate, getProfile, removeNextUpdate,
                                             setProfile, testReset)


----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: MonadWalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

isValidAddress :: MonadWalletWebMode m => CId Addr -> m Bool
isValidAddress = pure . isRight . cIdToAddress

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Get last update info
nextUpdate :: MonadWalletWebMode m => m CUpdateInfo
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
postponeUpdate :: MonadWalletWebMode m => m ()
postponeUpdate = removeNextUpdate

-- | Delete next update info and restart immediately
applyUpdate :: MonadWalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

syncProgress :: MonadWalletWebMode m => m SyncProgress
syncProgress =
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll :: MonadWalletWebMode m => m ()
testResetAll = deleteAllKeys >> testReset
  where
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0
