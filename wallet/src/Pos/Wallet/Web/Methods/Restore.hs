{-# LANGUAGE TypeFamilies #-}

-- | Everything related to wallet creation

module Pos.Wallet.Web.Methods.Restore
       ( newWalletHandler
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
import           Mockable (Mockable (..))
import           Mockable.Concurrent (Async)
import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Core.Configuration (genesisSecretsPoor)
import           Pos.Core.Genesis (poorSecretToEncKey)
import           Pos.Core.Txp (TxIn, TxOut (..), TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase, firstHardened)
import           Pos.DB (MonadDBRead)
import           Pos.StateLock (Priority (..), withStateLockNoMetrics)
import           Pos.Txp (utxoToModifier)
import           Pos.Txp.DB.Utxo (filterUtxo)
import           Pos.Util (maybeThrow)
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
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), MonadWalletDB, createAccount,
                                       getAccountWAddresses, getWalletMeta, removeHistoryCache,
                                       setWalletSyncTip, updateWalletBalancesAndUtxo)
import           Pos.Wallet.Web.Tracking.Decrypt (WalletDecrCredentials, decryptAddress,
                                                  eskToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (restoreGenesisAddresses, restoreWalletHistory)
import           Pos.Wallet.Web.Util (getWalletAccountIds)


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

newWallet
    :: L.MonadWalletLogic ctx m
    => PassPhrase -> CWalletInit -> Bool -> m (EncryptedSecretKey, CId Wal)
newWallet passphrase CWalletInit {..} isReady = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase cwBackupPhrase
    let cAddr = encToCId skey

    CWallet{..} <- L.createWalletSafe cAddr cwInitMeta isReady
    -- can't return this result, since balances can change

    let accMeta = CAccountMeta { caName = "Initial account" }
        accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    () <$ L.newAccountIncludeUnready True (DeterminedSeed initialAccAddrIdxs) passphrase accInit

    return (skey, cAddr)

newWalletHandler :: L.MonadWalletLogic ctx m => PassPhrase -> CWalletInit -> m CWallet
newWalletHandler passphrase cwInit = do
    -- A brand new wallet doesn't need any syncing, so we mark isReady=True
    (_, wId) <- newWallet passphrase cwInit True
    removeHistoryCache wId
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip wId tip
    L.getWallet wId

{- | Restores a wallet from a seed. The process is conceptually divided into
-- two parts:
-- 1. Recover this wallet balance from the global Utxo (fast, and synchronous);
-- 2. Recover the full transaction history from the blockchain (slow, asynchronous).
-}
restoreWalletFromSeed :: ( L.MonadWalletLogic ctx m
                        , Mockable Async m
                        ) => PassPhrase -> CWalletInit -> m CWallet
restoreWalletFromSeed passphrase cwInit = do
    (sk, _) <- newWallet passphrase cwInit False -- TODO(adn) readyness must be changed into richer type.
    restoreWallet sk

-- | Restores a Wallet by fetching by:
-- 1. Restoring the genesis addresses
-- 2. Restoring the balance (via Utxo)
-- 3. Restore (asynchronously) the histrory.
restoreWallet :: ( L.MonadWalletLogic ctx m
                 , Mockable Async m
                 ) => EncryptedSecretKey ->  m CWallet
restoreWallet sk = do
    let credentials@(_, wId) = eskToWalletDecrCredentials sk
    restoreGenesisAddresses credentials
    restoreWalletBalance credentials
    -- TODO(adn) Make restoreWalletHistory async.
    _ <- restoreWalletHistory sk
    WS.setWalletReady wId True
    -- TODO(adn) We can set the wallet ready much sooner, after 'restoreWalletHistory'
    -- is properly async.
    L.getWallet wId

-- | Restores the wallet balance by looking at the global Utxo and trying to decrypt
-- each unspent output address. If we get a match, it means it belongs to us.
restoreWalletBalance :: ( L.MonadWalletLogicRead ctx m
                        , MonadWalletDB ctx m
                        , MonadDBRead m
                        ) => WalletDecrCredentials -> m ()
restoreWalletBalance credentials = do
    utxo <- filterUtxo walletUtxo
    updateWalletBalancesAndUtxo (utxoToModifier utxo)
    where
      walletUtxo :: (TxIn, TxOutAux) -> Bool
      walletUtxo (_, TxOutAux (TxOut addr _)) =
          isJust (decryptAddress credentials addr)

restoreWalletFromBackup :: ( L.MonadWalletLogic ctx m
                           , Mockable Async m
                           ) => WalletBackup -> m CWallet
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey
    wExists <- isJust <$> getWalletMeta wId

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
                    accId <- genUniqueAccountId seedGen wId
                    createAccount accId meta

            -- TODO(adn): Review the readyness story.
            void $ L.createWalletSafe wId wMeta False

            -- Get wallet accounts and create default address for each account
            -- without any existing address
            wAccIds <- getWalletAccountIds wId
            for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
                Nothing -> throwM $ InternalError "restoreWalletFromBackup: fatal: cannot find \
                                                  \an existing account of newly imported wallet"
                Just [] -> void $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
                Just _  -> pure ()

            restoreWallet wbSecretKey

importWallet
    :: ( L.MonadWalletLogic ctx m
       , Mockable Async m
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
       , Mockable Async m
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
       , Mockable Async m
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

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress (DeterminedSeed accountIndex) passphrase accId

    restoreWallet key

-- | Creates wallet with given genesis hd-wallet key.
-- For debug purposes
addInitialRichAccount :: (L.MonadWalletLogic ctx m
                         , Mockable Async m
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
