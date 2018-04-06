{-# LANGUAGE TypeFamilies #-}

-- | Everything related to wallet creation

module Pos.Wallet.Web.Methods.Restore
       ( newWallet
       , importWallet
       , restoreWalletFromSeed
       , restoreWalletFromBackup
       , addInitialRichAccount

       -- For testing
       , importWalletDo
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Lens (each, ix, traversed)
import           Data.Default (Default (def))
import           Formatting (build, sformat, (%))
import           System.IO.Error (isDoesNotExistError)
import           System.Wlog (logDebug)

import qualified Data.HashMap.Strict as HM
import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Core.Configuration (genesisSecretsPoor)
import           Pos.Core.Genesis (poorSecretToEncKey)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase, firstHardened)
import           Pos.StateLock (Priority (..), withStateLockNoMetrics)
import           Pos.Util (HasLens (..), maybeThrow)
import           Pos.Util.UserSecret (UserSecretDecodingError (..), WalletUserSecret (..),
                                      mkGenesisWalletUserSecret, readUserSecret, usWallet,
                                      wusAccounts, wusWalletName)
import           Pos.Wallet.Web.Account (GenSeed (..), genSaveRootKey, genUniqueAccountId)
import           Pos.Wallet.Web.Backup (AccountMetaBackup (..), WalletBackup (..),
                                        WalletMetaBackup (..))
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountInit (..), CAccountMeta (..),
                                             CFilePath (..), CId, CWallet (..), CWalletInit (..),
                                             CWalletMeta (..), Wal, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), askWalletDB, askWalletSnapshot,
                                       createAccount, getAccountWAddresses, getWalletMeta,
                                       removeHistoryCache, setWalletSyncTip)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import qualified Pos.Wallet.Web.Tracking.Restore as Restore
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           UnliftIO (MonadUnliftIO)

-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

mkWallet
    :: L.MonadWalletLogic ctx m
    => PassPhrase -> CWalletInit -> Bool -> m (EncryptedSecretKey, CId Wal)
mkWallet passphrase CWalletInit {..} isReady = do
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
    db <- askWalletDB
    -- A brand new wallet doesn't need any syncing, so we mark isReady=True
    (_, wId) <- mkWallet passphrase cwInit True
    removeHistoryCache db wId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip db wId tip
    L.getWallet wId

{- | Restores a wallet from a seed. The process is conceptually divided into
-- two parts:
-- 1. Recover this wallet balance from the global Utxo (fast, and synchronous);
-- 2. Recover the full transaction history from the blockchain (slow, asynchronous).
-}
restoreWalletFromSeed :: ( L.MonadWalletLogic ctx m
                         , MonadUnliftIO m
                         , HasLens SyncQueue ctx SyncQueue
                         ) => PassPhrase -> CWalletInit -> m CWallet
restoreWalletFromSeed passphrase cwInit = do
    (sk, _) <- mkWallet passphrase cwInit False
    restoreWallet sk

restoreWallet :: ( L.MonadWalletLogic ctx m
                 , MonadUnliftIO m
                 , HasLens SyncQueue ctx SyncQueue
                 ) => EncryptedSecretKey -> m CWallet
restoreWallet sk = do
    db <- WS.askWalletDB
    let credentials@(_, wId) = eskToWalletDecrCredentials sk
    Restore.restoreWallet credentials
    WS.setWalletReady db wId True
    L.getWallet wId

restoreWalletFromBackup :: ( L.MonadWalletLogic ctx m
                           , MonadUnliftIO m
                           , HasLens SyncQueue ctx SyncQueue
                           ) => WalletBackup -> m CWallet
restoreWalletFromBackup WalletBackup {..} = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    let wId = encToCId wbSecretKey
    let wExists = isJust (getWalletMeta ws wId)

    if wExists
        then do
            throwM $ RequestError "Wallet with id already exists"

        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am
                defaultAccAddrIdx = DeterminedSeed firstHardened

            addSecretKey wbSecretKey
            -- If there are no existing accounts, then create one
            if null accList
                then do
                    let accMeta = CAccountMeta { caName = "Initial account" }
                        accInit = CAccountInit { caInitWId = wId, caInitMeta = accMeta }
                    () <$ L.newAccountIncludeUnready True defaultAccAddrIdx emptyPassphrase accInit
                else for_ accList $ \(idx, meta) -> do
                    let aIdx = fromInteger $ fromIntegral idx
                        seedGen = DeterminedSeed aIdx
                    accId <- genUniqueAccountId ws seedGen wId
                    createAccount db accId meta

            -- TODO(adn): Review the readyness story.
            void $ L.createWalletSafe wId wMeta False

            ws' <- askWalletSnapshot

            -- Get wallet accounts and create default address for each account
            -- without any existing address
            let wAccIds = getWalletAccountIds ws' wId
            for_ wAccIds $ \accId ->
                case getAccountWAddresses ws' Ever accId of
                    Nothing -> throwM $ InternalError "restoreWalletFromBackup: fatal: cannot find \
                                                      \an existing account of newly imported wallet"
                    Just [] -> void $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
                    Just _  -> pure ()

            restoreWallet wbSecretKey

importWallet
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
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
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletDo passphrase wSecret = do
    wId <- cwId <$> importWalletSecret emptyPassphrase wSecret
    _ <- L.changeWalletPassphrase wId emptyPassphrase passphrase
    L.getWallet wId

importWalletSecret
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret passphrase WalletUserSecret{..} = do
    let key    = _wusRootKey
        wid    = encToCId key
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    -- TODO(adinapoli): Review the readiness story.
    void $ L.createWalletSafe wid wMeta False

    db <- askWalletDB
    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        ws <- askWalletSnapshot
        cAddr <- genUniqueAccountId ws seedGen wid
        createAccount db cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress (DeterminedSeed accountIndex) passphrase accId

    restoreWallet key

-- | Creates wallet with given genesis hd-wallet key.
-- For debug purposes
addInitialRichAccount :: ( L.MonadWalletLogic ctx m
                         , MonadUnliftIO m
                         , HasLens SyncQueue ctx SyncQueue
                         ) => Int -> m ()
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
