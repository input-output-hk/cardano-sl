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
       , dumpState
       , WalletStateSnapshot (..)

       , resetAllFailedPtxs
       ) where

import           Universum

import           Data.Aeson                   (encode)
import           Data.Aeson.TH                (defaultOptions, deriveJSON)
import qualified Data.Text.Buildable
import           Formatting                   (build, sformat, (%))
import           Servant.API.ContentTypes     (MimeRender (..), OctetStream)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.Storage            ()
import           Pos.Aeson.Storage            ()
import           Pos.Core                     (SoftwareVersion (..))
import           Pos.Slotting                 (getCurrentSlotBlocking)
import           Pos.Update.Configuration     (curSoftwareVersion)
import           Pos.Util                     (maybeThrow)
import           Pos.Util.LogSafe             (logInfoUnsafeP)
import           Pos.Wallet.KeyStorage        (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode        (applyLastUpdate, connectedPeers,
                                               localChainDifficulty,
                                               networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes   (Addr, CId, CProfile (..), CUpdateInfo (..),
                                               SyncProgress (..), cIdToAddress)
import           Pos.Wallet.Web.Error         (WalletError (..))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.State         (getNextUpdate, getProfile,
                                               getWalletStorage, removeNextUpdate,
                                               resetFailedPtxs, setProfile, testReset)
import           Pos.Wallet.Web.State.Storage (WalletStorage)


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
syncProgress = do
    _spLocalCD <- localChainDifficulty
    _spNetworkCD <- networkChainDifficulty
    _spPeers <- connectedPeers
    -- servant already logs this, but only to secret logs
    logInfoUnsafeP $
        sformat ("Current sync progress: "%build%"/"%build)
        _spLocalCD _spNetworkCD
    return SyncProgress{..}

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll :: MonadWalletWebMode m => m ()
testResetAll = deleteAllKeys >> testReset
  where
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

dumpState :: MonadWalletWebMode m => m WalletStateSnapshot
dumpState = WalletStateSnapshot <$> getWalletStorage

----------------------------------------------------------------------------
-- Tx resubmitting
----------------------------------------------------------------------------

resetAllFailedPtxs :: MonadWalletWebMode m => m ()
resetAllFailedPtxs = getCurrentSlotBlocking >>= resetFailedPtxs
