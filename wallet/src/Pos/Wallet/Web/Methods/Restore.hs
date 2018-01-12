{-# LANGUAGE TypeFamilies #-}

-- | Everything related to wallet creation

module Pos.Wallet.Web.Methods.Restore
       ( newWallet
       , importWallet
       , restoreWallet
       , addInitialRichAccount

       -- For testing
       , importWalletDo
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Lens (ix, traversed)
import           Data.Default (Default (def))
import           Formatting (build, sformat, (%))
import           System.IO.Error (isDoesNotExistError)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Core.Configuration (genesisSecretsPoor)
import           Pos.Core.Genesis (poorSecretToEncKey)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase, firstHardened)
import           Pos.StateLock (Priority (..), withStateLockNoMetrics)
import           Pos.Util (maybeThrow)
import           Pos.Util.UserSecret (UserSecretDecodingError (..), WalletUserSecret (..),
                                      mkGenesisWalletUserSecret, readUserSecret, usWallet,
                                      wusAccounts, wusWalletName)
import           Pos.Wallet.Web.Account (GenSeed (..), genSaveRootKey, genUniqueAccountId)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountInit (..), CAccountMeta (..),
                                             CFilePath (..), CId, CWallet (..), CWalletInit (..),
                                             CWalletMeta (..), Wal, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.State (createAccount, removeHistoryCache, setWalletSyncTip)
import           Pos.Wallet.Web.Tracking (syncWalletOnImport)


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

newWalletFromBackupPhrase
    :: L.MonadWalletLogic ctx m
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

newWallet :: L.MonadWalletLogic ctx m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase cwInit = do
    -- A brand new wallet doesn't need any syncing, so we mark isReady=True
    (_, wId) <- newWalletFromBackupPhrase passphrase cwInit True
    removeHistoryCache wId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip wId tip
    L.getWallet wId

restoreWallet :: L.MonadWalletLogic ctx m => PassPhrase -> CWalletInit -> m CWallet
restoreWallet passphrase cwInit = do
    -- Restoring a wallet may take a long time.
    -- Hence we mark the wallet as "not ready" until `syncWalletOnImport` completes.
    (sk, wId) <- newWalletFromBackupPhrase passphrase cwInit False
    -- `syncWalletOnImport` automatically marks a wallet as "ready".
    syncWalletOnImport sk
    L.getWallet wId

importWallet
    :: L.MonadWalletLogic ctx m
    => PassPhrase
    -> CFilePath
    -> m CWallet
importWallet passphrase (CFilePath (toString -> fp)) = do
    secret <-
        rewrapToWalletError isDoesNotExistError noFile $
        rewrapToWalletError (\UserSecretDecodingError{} -> True) decodeFailed $
        readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWallet)
    importWalletDo passphrase wSecret
  where
    noWalletSecret = RequestError "This key doesn't contain HD wallet info"
    noFile _ = RequestError "File doesn't exist"
    decodeFailed = RequestError . sformat ("Invalid secret file ("%build%")")

-- Do the all concrete logic of importing here.
importWalletDo
    :: L.MonadWalletLogic ctx m
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletDo passphrase wSecret = do
    wId <- cwId <$> importWalletSecret emptyPassphrase wSecret
    _ <- L.changeWalletPassphrase wId emptyPassphrase passphrase
    L.getWallet wId

importWalletSecret
    :: L.MonadWalletLogic ctx m
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

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress (DeterminedSeed accountIndex) passphrase accId

    -- `syncWalletOnImport` automatically marks a wallet as "ready".
    void $ syncWalletOnImport key

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
-- For debug purposes
addInitialRichAccount :: L.MonadWalletLogic ctx m => Int -> m ()
addInitialRichAccount keyId =
    E.handleAny wSetExistsHandler $ do
        let hdwSecretKeys = fromMaybe (error "Hdw secrets keys are unknown") genesisSecretsPoor
        key <- maybeThrow noKey (map poorSecretToEncKey $ hdwSecretKeys ^? ix keyId)
        void $ importWalletSecret emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")
