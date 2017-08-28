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
import           Pos.Crypto                   (EncryptedSecretKey, PassPhrase,
                                               emptyPassphrase, firstHardened)
import           Pos.Genesis                  (genesisDevHdwSecretKeys)
import           Pos.StateLock                (withStateLock)
import           Pos.Util                     (maybeThrow)
import           Pos.Util.UserSecret          (UserSecretDecodingError (..),
                                               readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage        (addSecretKey)
import           Pos.Wallet.Web.Account       (GenSeed (..), genSaveRootKey,
                                               genUniqueAccountId)
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CAccountInit (..),
                                               CAccountMeta (..), CId, CWallet (..),
                                               CWalletInit (..), CWalletMeta (..), Wal,
                                               encToCId)
import           Pos.Wallet.Web.Error         (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret        (WalletUserSecret (..),
                                               mkGenesisWalletUserSecret, wusAccounts,
                                               wusWalletName)
import           Pos.Wallet.Web.State         (createAccount, setWalletSyncTip,
                                               updateHistoryCache)
import           Pos.Wallet.Web.Tracking      (syncWalletOnImport)


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

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
    withStateLock $ \tip -> setWalletSyncTip wId tip
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
-- For debug purposes
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
