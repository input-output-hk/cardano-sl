{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Various small endpoints

module Pos.Wallet.Web.Methods.Misc
       ( getUserProfile
       , updateUserProfile

       , isValidAddress

       , nextUpdate
       , applyUpdate

       , syncProgress

       , testResetAll
       ) where

import           Universum

import           Control.Lens                   (has)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as S
import           Formatting                     (build, sformat, (%))
import qualified Formatting                     as F
import           System.Wlog                    (logDebug, logInfo)

import           Pos.Aeson.ClientTypes          ()
import           Pos.Aeson.WalletBackup         ()
import           Pos.Binary.Class               (biSize)
import           Pos.Client.Txp.Balances        (getOwnUtxos)
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Client.Txp.Util            (TxError (..), createMTx,
                                                 overrideTxDistrBoot,
                                                 overrideTxOutDistrBoot)
import           Pos.Communication              (SendActions (..), submitMTx)
import           Pos.Core                       (Coin, TxFeePolicy (..),
                                                 TxSizeLinear (..), addressF,
                                                 bvdTxFeePolicy, calculateTxSizeLinear,
                                                 decodeTextAddress, getCurrentTimestamp,
                                                 integerToCoin, mkCoin, unsafeAddCoin,
                                                 unsafeSubCoin, _RedeemAddress)
import           Pos.Crypto                     (PassPhrase, fakeSigner, hash, keyGen,
                                                 withSafeSigners)
import           Pos.DB.Class                   (gsAdoptedBVData)
import           Pos.Txp                        (TxFee (..))
import           Pos.Txp.Core                   (TxAux (..), TxOut (..), TxOutAux (..),
                                                 TxOutDistribution)
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Wallet.KeyStorage          (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode          (applyLastUpdate, connectedPeers,
                                                 localChainDifficulty,
                                                 networkChainDifficulty)
import           Pos.Wallet.Web.Account         (GenSeed (..), MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CAddress (..),
                                                 CCoin, CId, CProfile, CProfile (..),
                                                 CTx (..), CTxs (..), CUpdateInfo (..),
                                                 CWAddressMeta (..), SyncProgress (..),
                                                 Wal, addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.State           (AddressLookupMode (Existing),
                                                 getNextUpdate, getProfile,
                                                 removeNextUpdate, setProfile, testReset)
import           Pos.Wallet.Web.Util            (getWalletAccountIds, rewrapTxError)


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
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

applyUpdate :: MonadWalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

syncProgress :: MonadWalletWebMode m => m SyncProgress
syncProgress = do
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
