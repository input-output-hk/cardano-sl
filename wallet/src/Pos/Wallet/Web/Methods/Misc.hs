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

       , PendingTxsSummary (..)
       , gatherPendingTxsSummary
       , resetAllFailedPtxs
       , cancelAllApplyingPtxs
       , cancelOneApplyingPtx
       ) where

import           Universum

import           Data.Aeson                   (encode)
import           Data.Aeson.TH                (defaultOptions, deriveJSON)
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Serokell.Util.Text           (listJson)
import           Servant.API.ContentTypes     (MimeRender (..), OctetStream)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Core                     (SlotId, SoftwareVersion (..),
                                               decodeTextAddress)
import           Pos.Slotting                 (getCurrentSlotBlocking)
import           Pos.Txp                      (Tx (..), TxAux (..), TxIn, TxOut)
import           Pos.Update.Configuration     (curSoftwareVersion)
import           Pos.Util                     (maybeThrow)
import           Pos.Util.Servant             (HasTruncateLogPolicy (..), encodeCType)

import           Pos.Aeson.Storage            ()
import           Pos.Util.Chrono              (getNewestFirst, toNewestFirst)
import           Pos.Wallet.KeyStorage        (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode        (applyLastUpdate, connectedPeers,
                                               localChainDifficulty,
                                               networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes   (CProfile (..), CPtxCondition, CTxId (..),
                                               CUpdateInfo (..), SyncProgress (..))
import           Pos.Wallet.Web.Error         (WalletError (..))
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Pending       (PendingTx (..), isPtxInBlocks,
                                               sortPtxsChrono)
import           Pos.Wallet.Web.State         (cancelApplyingPtxs,
                                               cancelSpecificApplyingPtx, getNextUpdate,
                                               getPendingTxs, getProfile,
                                               getWalletStorage, removeNextUpdate,
                                               resetFailedPtxs, setProfile, testReset)
import           Pos.Wallet.Web.State.Storage (WalletStorage)
import           Pos.Wallet.Web.Util          (decodeCTypeOrFail, testOnlyEndpoint)


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

-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: MonadWalletWebMode m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

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
testResetAll = testOnlyEndpoint $ deleteAllKeys >> testReset
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
-- Pending txs
----------------------------------------------------------------------------

data PendingTxsSummary = PendingTxsSummary
    { ptiSlot    :: !SlotId
    , ptiCond    :: !CPtxCondition
    , ptiInputs  :: !(NonEmpty TxIn)
    , ptiOutputs :: !(NonEmpty TxOut)
    } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''PendingTxsSummary

instance Buildable PendingTxsSummary where
    build PendingTxsSummary{..} =
        bprint (  "  slotId: "%build%
                "\n  status: "%build%
                "\n  inputs: "%listJson%
                "\n  outputs: "%listJson)
            ptiSlot
            ptiCond
            ptiInputs
            ptiOutputs

instance HasTruncateLogPolicy PendingTxsSummary where
    -- called rarely, and we are very interested in the output
    truncateLogPolicy = identity

gatherPendingTxsSummary :: MonadWalletWebMode m => m [PendingTxsSummary]
gatherPendingTxsSummary =
    map mkInfo .
    getNewestFirst . toNewestFirst . sortPtxsChrono .
    filter unconfirmedPtx <$>
    getPendingTxs
  where
    unconfirmedPtx = not . isPtxInBlocks . _ptxCond
    mkInfo PendingTx{..} =
        let tx = taTx _ptxTxAux
        in  PendingTxsSummary
            { ptiSlot = _ptxCreationSlot
            , ptiCond = encodeCType (Just _ptxCond)
            , ptiInputs = _txInputs tx
            , ptiOutputs = _txOutputs tx
            }

resetAllFailedPtxs :: MonadWalletWebMode m => m ()
resetAllFailedPtxs =
    testOnlyEndpoint $ getCurrentSlotBlocking >>= resetFailedPtxs

cancelAllApplyingPtxs :: MonadWalletWebMode m => m ()
cancelAllApplyingPtxs = testOnlyEndpoint cancelApplyingPtxs

cancelOneApplyingPtx :: MonadWalletWebMode m => CTxId -> m ()
cancelOneApplyingPtx cTxId = do
    txId <- decodeCTypeOrFail cTxId
    testOnlyEndpoint (cancelSpecificApplyingPtx txId)
