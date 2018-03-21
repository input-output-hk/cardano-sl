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

       , requestShutdown

       , testResetAll
       , dumpState
       , WalletStateSnapshot (..)

       , resetAllFailedPtxs

       , MonadConvertToAddr
       , convertCIdTOAddrs
       , convertCIdTOAddr
       , AddrCIdHashes(AddrCIdHashes)
       , PendingTxsSummary (..)
       , cancelAllApplyingPtxs
       , cancelOneApplyingPtx
       ) where

import           Universum

import           Data.Aeson (encode)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable
import           Data.Time.Units (toMicroseconds)
import           Formatting (bprint, build, sformat, (%))
import           Mockable (Delay, LowLevelAsync, Mockables, MonadMockable, async, delay)
import           Serokell.Util (listJson, sec)
import           Servant.API.ContentTypes (MimeRender (..), NoContent (..), OctetStream)
import           System.Wlog (WithLogger)

import           Pos.Client.KeyStorage (MonadKeys (..), deleteAllSecretKeys)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (Address, HasConfiguration, SlotId, SoftwareVersion (..))
import           Pos.Crypto (hashHexF)
import           Pos.NtpCheck (NtpCheckMonad, NtpStatus (..), getNtpStatusOnce)
import           Pos.Shutdown (HasShutdownContext, triggerShutdown)
import           Pos.Slotting (MonadSlots, getCurrentSlotBlocking)
import           Pos.Txp (TxId, TxIn, TxOut)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util (HasLens, lensOf, maybeThrow)
import           Pos.Util.LogSafe (logInfoUnsafeP)
import           Pos.Util.Servant (HasTruncateLogPolicy (..))
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.Storage ()
import           Pos.Wallet.WalletMode (MonadBlockchainInfo, MonadUpdates, applyLastUpdate,
                                        connectedPeers, localChainDifficulty,
                                        networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (Addr, CHash, CId (..), CProfile (..), CPtxCondition,
                                             CTxId (..), CUpdateInfo (..), SyncProgress (..),
                                             cIdToAddress)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (WalletDbReader, WalletSnapshot, askWalletDB,
                                       askWalletSnapshot, cancelApplyingPtxs,
                                       cancelSpecificApplyingPtx, getNextUpdate, getProfile,
                                       removeNextUpdate, resetFailedPtxs,
                                       setProfile, testReset)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, testOnlyEndpoint)

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: (WalletDbReader ctx m, MonadIO m) => m CProfile
getUserProfile = getProfile <$> askWalletSnapshot

updateUserProfile :: (HasConfiguration, WalletDbReader ctx m, MonadIO m)
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
       , HasConfiguration
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
postponeUpdate :: (MonadIO m, HasConfiguration, WalletDbReader ctx m) => m NoContent
postponeUpdate = askWalletDB >>= removeNextUpdate >> return NoContent

-- | Delete next update info and restart immediately
applyUpdate :: ( MonadIO m
               , HasConfiguration
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
       ( HasConfiguration
       , MonadIO m
       , MonadReader ctx m
       , WithLogger m
       , HasShutdownContext ctx
       , Mockables m [Delay, LowLevelAsync]
       )
    => m NoContent
requestShutdown = NoContent <$ async (delay (sec 1) >> triggerShutdown)

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

localTimeDifference :: (NtpCheckMonad m, MonadMockable m) => m Integer
localTimeDifference =
    diff <$> getNtpStatusOnce
  where
    diff :: NtpStatus -> Integer
    diff = \case
        NtpSyncOk -> 0
        -- `NtpSyncOk` considered already a `timeDifferenceWarnThreshold`
        -- so that we can return 0 here to show there is no difference in time
        NtpDesync diff' -> toMicroseconds diff'

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll ::
       ( HasConfiguration, HasNodeConfiguration, MonadIO m
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

resetAllFailedPtxs :: (HasConfiguration, MonadSlots ctx m, WalletDbReader ctx m) => m NoContent
resetAllFailedPtxs = do
    db <- askWalletDB
    getCurrentSlotBlocking >>= resetFailedPtxs db
    return NoContent

----------------------------------------------------------------------------
-- Conversion to Address
----------------------------------------------------------------------------

newtype AddrCIdHashes = AddrCIdHashes { unAddrCIdHashes :: (IORef (Map CHash Address)) }

type MonadConvertToAddr ctx m =
  ( MonadIO m
  , MonadThrow m
  , HasLens AddrCIdHashes ctx AddrCIdHashes
  , MonadReader ctx m
  )

convertCIdTOAddr :: (MonadConvertToAddr ctx m) => CId Addr -> m Address
convertCIdTOAddr i@(CId id) = do
    hmRef <- unAddrCIdHashes <$> view (lensOf @AddrCIdHashes)
    maddr <- atomicModifyIORef' hmRef $ \hm ->
      case id `M.lookup` hm of
       Just addr -> (hm, Right addr)
       _         -> case cIdToAddress i of
                    -- decoding can fail, but we don't cache failures
                      Right addr -> (M.insert id addr hm, Right addr)
                      Left  err  -> (hm,                  Left err)
    either (throwM . DecodeError) pure maddr

convertCIdTOAddrs :: (MonadConvertToAddr ctx m, Traversable t) => t (CId Addr) -> m (t Address)
convertCIdTOAddrs cids = do
    hmRef <- unAddrCIdHashes <$> view (lensOf @AddrCIdHashes)
    maddrs <- atomicModifyIORef' hmRef $ \hm ->
      let lookups = map (\cid@(CId h) -> (h, M.lookup h hm, cIdToAddress cid)) cids
          hm'     = Foldable.foldl' accum hm lookups

          accum m (cid, Nothing, Right addr) = M.insert cid addr m
          accum m _                          = m

          result (_, Just addr, _)   = Right addr
          result (_, Nothing, maddr) = maddr

       in (hm', map result lookups)

    mapM (either (throwM . DecodeError) pure) maddrs

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
    :: ( HasConfiguration
       , HasNodeConfiguration
       , MonadIO m
       , MonadThrow m
       , WalletDbReader ctx m
       )
    => m NoContent
cancelAllApplyingPtxs = do
  db <- askWalletDB
  testOnlyEndpoint $ NoContent <$ cancelApplyingPtxs db

cancelOneApplyingPtx ::
       ( HasConfiguration
       , HasNodeConfiguration
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
