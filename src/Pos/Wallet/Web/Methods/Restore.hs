{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods where

import           Universum

import           Control.Lens                   (has, ix, traversed)
import           Control.Monad.Catch            (SomeException, try)
import qualified Control.Monad.Catch            as E
import           Data.ByteString.Base58         (bitcoinAlphabet, decodeBase58)
import           Data.Default                   (Default (def))
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as S
import           Formatting                     (build, sformat, shown, (%))
import qualified Formatting                     as F
import           Pos.ReportServer.Report        (ReportType (RInfo))
import qualified Serokell.Util.Base64           as B64
import           Servant.Multipart              (fdFilePath)
import           System.IO.Error                (isDoesNotExistError)
import           System.Wlog                    (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes          ()
import           Pos.Aeson.WalletBackup         ()
import           Pos.Binary.Class               (biSize)
import           Pos.Block.Logic.Util           (withBlkSemaphore_)
import           Pos.Client.Txp.Balances        (getOwnUtxos)
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Client.Txp.Util            (TxError (..), createMTx,
                                                 overrideTxDistrBoot,
                                                 overrideTxOutDistrBoot)
import           Pos.Communication              (SendActions (..), submitMTx,
                                                 submitRedemptionTx)
import           Pos.Constants                  (isDevelopment)
import           Pos.Core                       (Coin, TxFeePolicy (..),
                                                 TxSizeLinear (..), addressF,
                                                 bvdTxFeePolicy, calculateTxSizeLinear,
                                                 decodeTextAddress, getCurrentTimestamp,
                                                 integerToCoin, makeRedeemAddress, mkCoin,
                                                 unsafeAddCoin, unsafeSubCoin,
                                                 _RedeemAddress)
import           Pos.Crypto                     (EncryptedSecretKey, PassPhrase,
                                                 aesDecrypt, deriveAesKeyBS,
                                                 emptyPassphrase, fakeSigner, hash,
                                                 keyGen, redeemDeterministicKeyGen,
                                                 redeemToPublic, withSafeSigners)
import           Pos.DB.Class                   (gsAdoptedBVData)
import           Pos.Genesis                    (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState         (HasReportServers (..),
                                                 HasReportingContext (..))
import           Pos.Reporting.Methods          (sendReport, sendReportNodeNologs)
import           Pos.Txp                        (TxFee (..))
import           Pos.Txp.Core                   (TxAux (..), TxOut (..), TxOutAux (..),
                                                 TxOutDistribution)
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase          (toSeed)
import           Pos.Util.UserSecret            (UserSecretDecodingError (..),
                                                 readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage          (addSecretKey, deleteSecretKey,
                                                 getSecretKeys)
import           Pos.Wallet.WalletMode          (applyLastUpdate, connectedPeers,
                                                 localChainDifficulty,
                                                 networkChainDifficulty)
import           Pos.Wallet.Web.Account         (GenSeed (..), MonadKeySearch (..),
                                                 genSaveRootKey, genUniqueAccountId)
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CAccountId (..),
                                                 CAccountInit (..), CAccountMeta (..),
                                                 CAddress (..), CCoin,
                                                 CElectronCrashReport (..), CId,
                                                 CInitialized,
                                                 CPaperVendWalletRedeem (..), CProfile,
                                                 CProfile (..), CTx (..), CTxId,
                                                 CTxMeta (..), CTxs (..),
                                                 CUpdateInfo (..), CWAddressMeta (..),
                                                 CWallet (..), CWalletInit (..),
                                                 CWalletMeta (..), CWalletRedeem (..),
                                                 SyncProgress (..), Wal,
                                                 addrMetaToAccount, encToCId, mkCCoin)
import           Pos.Wallet.Web.Error           (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret          (WalletUserSecret (..),
                                                 mkGenesisWalletUserSecret, wusAccounts,
                                                 wusWalletName)
import           Pos.Wallet.Web.State           (AddressLookupMode (Existing),
                                                 createAccount, getNextUpdate, getProfile,
                                                 removeNextUpdate, setProfile,
                                                 setWalletSyncTip, setWalletTxMeta,
                                                 testReset, updateHistoryCache)
import           Pos.Wallet.Web.Tracking        (fixingCachedAccModifier,
                                                 syncWalletOnImport)
import           Pos.Wallet.Web.Util            (getWalletAccountIds, rewrapTxError)


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = 0

newWalletFromBackupPhrase
    :: MonadWalletWebMode m
    => PassPhrase -> CWalletInit -> m (EncryptedSecretKey, CId Wal)
newWalletFromBackupPhrase passphrase CWalletInit {..} = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase cwBackupPhrase
    let cAddr = encToCId skey

    CWallet{..} <- L.createWalletSafe cAddr cwInitMeta
    -- can't return this result, since balances can change

    let accMeta = CAccountMeta { caName = "Initial account" }
        accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    () <$ L.newAccount (DeterminedSeed initialAccAddrIdxs) passphrase accInit

    return (skey, cAddr)

newWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase cwInit = do
    (_, wId) <- newWalletFromBackupPhrase passphrase cwInit
    updateHistoryCache wId []
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withBlkSemaphore_ $ \tip -> tip <$ setWalletSyncTip wId tip
    L.getWallet wId

restoreWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
restoreWallet passphrase cwInit = do
    (sk, wId) <- newWalletFromBackupPhrase passphrase cwInit
    syncWalletOnImport sk
    L.getWallet wId

importWallet
    :: MonadWalletWebMode m
    => PassPhrase
    -> Text
    -> m CWallet
importWallet passphrase (toString -> fp) = do
    secret <-
        rewrapToWalletError isDoesNotExistError noFile $
        rewrapToWalletError (\UserSecretDecodingError{} -> True) decodeFailed $
        readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWalletSet)
    wId <- cwId <$> importWalletSecret emptyPassphrase wSecret
    L.changeWalletPassphrase wId emptyPassphrase passphrase
    L.getWallet wId
  where
    noWalletSecret = RequestError "This key doesn't contain HD wallet info"
    noFile _ = RequestError "File doesn't exist"
    decodeFailed = RequestError . sformat ("Invalid secret file ("%build%")")

importWalletSecret
    :: MonadWalletWebMode m
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret passphrase WalletUserSecret{..} = do
    let key    = _wusRootKey
        wid    = encToCId key
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    importedWallet <- L.createWalletSafe wid wMeta

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress (DeterminedSeed accountIndex) passphrase accId

    void $ syncWalletOnImport key

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
addInitialRichAccount :: MonadWalletWebMode m => Int -> m ()
addInitialRichAccount keyId =
    when isDevelopment . E.handleAll wSetExistsHandler $ do
        key <- maybeThrow noKey (genesisDevHdwSecretKeys ^? ix keyId)
        void $ importWalletSecret emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")
