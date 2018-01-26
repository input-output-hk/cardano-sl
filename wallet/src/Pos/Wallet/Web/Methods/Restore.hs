{-# LANGUAGE TypeFamilies #-}

-- | Everything related to wallet creation

module Pos.Wallet.Web.Methods.Restore
       ( newWallet
       , importWallet
       , restoreWallet
       , addInitialRichAccount
       ) where

import           Universum

import           Control.Lens                 (ix, traversed)
import qualified Control.Monad.Catch          as E
import           Data.Default                 (Default (def))
import           Formatting                   (build, sformat, (%))
import           System.IO.Error              (isDoesNotExistError)
import           System.Wlog                  (logDebug)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Constants                (isDevelopment)
import           Pos.Core.Configuration       (genesisHdwSecretKeys)
import           Pos.Crypto                   (EncryptedSecretKey, PassPhrase,
                                               emptyPassphrase, firstHardened)
import           Pos.StateLock                (Priority (..), withStateLockNoMetrics)
import           Pos.Util                     (maybeThrow)
import           Pos.Util.UserSecret          (UserSecretDecodingError (..),
                                               readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage        (addSecretKey)
import           Pos.Wallet.Web.Account       (GenSeed (..), genSaveRootKey,
                                               genUniqueAccountId)
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CAccountInit (..),
                                               CAccountMeta (..), CFilePath (..), CId,
                                               CWallet (..), CWalletInit (..),
                                               CWalletMeta (..), Wal, encToCId)
import           Pos.Wallet.Web.Error         (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret        (WalletUserSecret (..),
                                               mkGenesisWalletUserSecret, wusAccounts,
                                               wusWalletName)
import           Pos.Wallet.Web.State         (askWalletDB, askWalletSnapshot, createAccount,
                                               removeHistoryCache, setWalletSyncTip)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

newWalletFromBackupPhrase
    :: MonadWalletWebMode m
    => PassPhrase -> CWalletInit -> Bool -> m (EncryptedSecretKey, CId Wal)
newWalletFromBackupPhrase passphrase CWalletInit {..} isReady = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase cwBackupPhrase
    let cAddr = encToCId skey

    CWallet{..} <- L.createWalletSafe cAddr cwInitMeta isReady
    -- can't return this result, since balances can change

    let accMeta = CAccountMeta { caName = "Initial account" }
        accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    () <$ L.newAccountIncludeUnready True (DeterminedSeed initialAccAddrIdxs) passphrase accInit

    return (skey, cAddr)

newWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase cwInit = do
    db <- askWalletDB
    -- A brand new wallet doesn't need any syncing, so we mark isReady=True
    (_, wId) <- newWalletFromBackupPhrase passphrase cwInit True
    removeHistoryCache db wId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip db wId tip
    L.getWallet wId

restoreWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
restoreWallet passphrase cwInit = do
    -- Restoring a wallet may take a long time.
    -- Hence we mark the wallet as "not ready" until `syncWalletOnImport` completes.
    (sk, wId) <- newWalletFromBackupPhrase passphrase cwInit False
    -- `syncWalletOnImport` automatically marks a wallet as "ready".
    syncWalletOnImport sk
    L.getWallet wId

importWallet
    :: MonadWalletWebMode m
    => PassPhrase
    -> CFilePath
    -> m CWallet
importWallet passphrase (CFilePath (toString -> fp)) = do
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
    -- Importing a wallet may take a long time.
    -- Hence we mark the wallet as "not ready" until `syncWalletOnImport` completes.
    importedWallet <- L.createWalletSafe wid wMeta False

    db <- askWalletDB
    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        ws <- askWalletSnapshot
        cAddr <- genUniqueAccountId ws seedGen wid
        createAccount db cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        ws <- askWalletSnapshot
        L.newAddress ws (DeterminedSeed accountIndex) passphrase accId

    -- `syncWalletOnImport` automatically marks a wallet as "ready".
    void $ syncWalletOnImport key

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
-- For debug purposes
addInitialRichAccount :: MonadWalletWebMode m => Int -> m ()
addInitialRichAccount keyId =
    when isDevelopment . E.handleAll wSetExistsHandler $ do
        let hdwSecretKeys = fromMaybe (error "Hdw secrets keys are unknown") genesisHdwSecretKeys
        key <- maybeThrow noKey (hdwSecretKeys ^? ix keyId)
        void $ importWalletSecret emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")
