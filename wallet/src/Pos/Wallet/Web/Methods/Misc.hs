{-# LANGUAGE DataKinds    #-}
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
       , localTimeDifferencePure

       , requestShutdown

       , testResetAll
       , dumpState
       , WalletStateSnapshot (..)

       , resetAllFailedPtxs

       , PendingTxsSummary (..)
       , cancelAllApplyingPtxs
       , cancelOneApplyingPtx
       ) where

import           Universum

import           Data.Aeson (encode)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Time.Units (Second, toMicroseconds)
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)
import           Servant.API.ContentTypes (MimeRender (..), NoContent (..),
                     OctetStream)
import           UnliftIO (MonadUnliftIO)

import           Ntp.Client (NtpStatus (..))

import           Pos.Chain.Txp (TxId, TxIn, TxOut)
import           Pos.Chain.Update (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Client.KeyStorage (MonadKeys (..), deleteAllSecretKeys)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (ProtocolConstants, SlotId, pcEpochSlots)
import           Pos.Core.Conc (async, delay)
import           Pos.Core.Update (SoftwareVersion (..))
import           Pos.Crypto (hashHexF)
import           Pos.Infra.Shutdown (HasShutdownContext, triggerShutdown)
import           Pos.Infra.Slotting (MonadSlots, getCurrentSlotBlocking)
import           Pos.Infra.Util.LogSafe (logInfoUnsafeP)
import           Pos.Util (maybeThrow)
import           Pos.Util.Servant (HasTruncateLogPolicy (..))
import           Pos.Util.Wlog (WithLogger)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.Storage ()
import           Pos.Wallet.WalletMode (MonadBlockchainInfo, MonadUpdates,
                     applyLastUpdate, connectedPeers, localChainDifficulty,
                     networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (Addr, CId (..), CProfile (..),
                     CPtxCondition, CTxId (..), CUpdateInfo (..),
                     SyncProgress (..), cIdToAddress)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (WalletDbReader, WalletSnapshot,
                     askWalletDB, askWalletSnapshot, cancelApplyingPtxs,
                     cancelSpecificApplyingPtx, getNextUpdate, getProfile,
                     removeNextUpdate, resetFailedPtxs, setProfile, testReset)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, testOnlyEndpoint)

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: (WalletDbReader ctx m, MonadIO m) => m CProfile
getUserProfile = getProfile <$> askWalletSnapshot

updateUserProfile :: (WalletDbReader ctx m, MonadIO m)
                  => CProfile
                  -> m CProfile
updateUserProfile profile = do
    db <- askWalletDB
    setProfile db profile
    getUserProfile

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
    :: ( MonadIO m
       , MonadThrow m
       , WalletDbReader ctx m
       , HasUpdateConfiguration
       )
    => m CUpdateInfo
nextUpdate = do
    ws <- askWalletSnapshot
    updateInfo <- maybeThrow noUpdates (getNextUpdate ws)
    if isUpdateActual (cuiSoftwareVersion updateInfo)
        then pure updateInfo
        else askWalletDB >>= removeNextUpdate >> nextUpdate
        --TODO: this should be a single transaction
  where
    isUpdateActual :: SoftwareVersion -> Bool
    isUpdateActual ver = svAppName ver == svAppName curSoftwareVersion
        && svNumber ver > svNumber curSoftwareVersion
    noUpdates = RequestError "No updates available"

-- | Postpone next update after restart
postponeUpdate :: (MonadIO m, WalletDbReader ctx m) => m NoContent
postponeUpdate = askWalletDB >>= removeNextUpdate >> return NoContent

-- | Delete next update info and restart immediately
applyUpdate :: ( MonadIO m
               , WalletDbReader ctx m
               , MonadUpdates m
               )
            => m NoContent
applyUpdate = askWalletDB >>= removeNextUpdate
              >> applyLastUpdate >> return NoContent

----------------------------------------------------------------------------
-- System
----------------------------------------------------------------------------

-- | Triggers shutdown in a short interval after called. Delay is
-- needed in order for http request to succeed.
requestShutdown ::
       ( MonadIO m
       , MonadUnliftIO m
       , MonadReader ctx m
       , WithLogger m
       , HasShutdownContext ctx
       )
    => m NoContent
requestShutdown = NoContent <$ async (delay (1 :: Second) >> triggerShutdown)

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

syncProgress
    :: (MonadIO m, WithLogger m, MonadBlockchainInfo m)
    => m SyncProgress
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
-- NTP (Network Time Protocol) based time difference
----------------------------------------------------------------------------

localTimeDifferencePure :: NtpStatus -> Maybe Integer
localTimeDifferencePure (NtpDrift time)    = Just (toMicroseconds time)
localTimeDifferencePure NtpSyncPending     = Nothing
localTimeDifferencePure NtpSyncUnavailable = Nothing

localTimeDifference :: MonadIO m => TVar NtpStatus -> m (Maybe Integer)
localTimeDifference ntpStatus = localTimeDifferencePure <$> readTVarIO ntpStatus

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll ::
       ( HasNodeConfiguration, MonadIO m
       , MonadThrow m, WalletDbReader ctx m, MonadKeys m)
    => m NoContent
testResetAll = do
    db <- askWalletDB
    testOnlyEndpoint $ deleteAllSecretKeys >> testReset db >> return NoContent

----------------------------------------------------------------------------
-- Print wallet state
----------------------------------------------------------------------------

data WalletStateSnapshot = WalletStateSnapshot
    { wssWalletStorage :: WalletSnapshot
    } deriving (Generic)

deriveJSON defaultOptions ''WalletStateSnapshot

instance MimeRender OctetStream WalletStateSnapshot where
    mimeRender _ = encode

instance Buildable WalletStateSnapshot where
    build _ = "<wallet-state-snapshot>"

dumpState :: (MonadIO m, WalletDbReader ctx m)
          => m WalletStateSnapshot
dumpState = WalletStateSnapshot <$> askWalletSnapshot

----------------------------------------------------------------------------
-- Tx resubmitting
----------------------------------------------------------------------------

resetAllFailedPtxs
    :: (MonadSlots ctx m, WalletDbReader ctx m)
    => ProtocolConstants
    -> m NoContent
resetAllFailedPtxs pc = do
    db <- askWalletDB
    getCurrentSlotBlocking (pcEpochSlots pc) >>= resetFailedPtxs pc db
    return NoContent

----------------------------------------------------------------------------
-- Print pending transactions info
----------------------------------------------------------------------------

data PendingTxsSummary = PendingTxsSummary
    { ptiSlot    :: !SlotId
    , ptiCond    :: !CPtxCondition
    , ptiInputs  :: !(NonEmpty TxIn)
    , ptiOutputs :: !(NonEmpty TxOut)
    , ptiTxId    :: !TxId
    } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''PendingTxsSummary

instance Buildable PendingTxsSummary where
    build PendingTxsSummary{..} =
        bprint (  "  slotId: "%build%
                "\n  status: "%build%
                "\n  inputs: "%listJson%
                "\n  outputs: "%listJson%
                "\n  id: "%hashHexF)
            ptiSlot
            ptiCond
            ptiInputs
            ptiOutputs
            ptiTxId

instance HasTruncateLogPolicy PendingTxsSummary where
    -- called rarely, and we are very interested in the output
    truncateLogPolicy = identity

cancelAllApplyingPtxs
    :: ( HasNodeConfiguration
       , MonadIO m
       , MonadThrow m
       , WalletDbReader ctx m
       )
    => m NoContent
cancelAllApplyingPtxs = do
  db <- askWalletDB
  testOnlyEndpoint $ NoContent <$ cancelApplyingPtxs db

cancelOneApplyingPtx ::
       ( HasNodeConfiguration
       , MonadThrow m
       , WalletDbReader ctx m
       , MonadIO m
       )
    => CTxId
    -> m NoContent
cancelOneApplyingPtx cTxId = testOnlyEndpoint $ NoContent <$ do
    db <- askWalletDB
    txId <- decodeCTypeOrFail cTxId
    cancelSpecificApplyingPtx db txId
