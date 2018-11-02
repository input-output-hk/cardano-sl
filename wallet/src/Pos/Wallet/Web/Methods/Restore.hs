{-# LANGUAGE TypeFamilies #-}

-- | Everything related to wallet creation

module Pos.Wallet.Web.Methods.Restore
       ( newWallet
       , newWalletNoThrow
       , importWallet
       , restoreWalletFromSeed
       , restoreWalletFromSeedNoThrow
       , restoreWalletFromBackup
       , addInitialRichAccount

       -- For testing
       , importWalletDo
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Lens (each, ix, traversed)
import           Data.Default (Default (def))
import           Data.Traversable (for)
import           Formatting (build, sformat, (%))
import           System.IO.Error (isDoesNotExistError)

import qualified Data.HashMap.Strict as HM
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Genesis (PoorSecret, poorSecretToEncKey)
import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase,
                     firstHardened)
import           Pos.Infra.StateLock (Priority (..), withStateLockNoMetrics)
import           Pos.Util (HasLens (..), maybeThrow)
import           Pos.Util.UserSecret (UserSecretDecodingError (..),
                     WalletUserSecret (..), mkGenesisWalletUserSecret,
                     readUserSecret, usWallet, wusAccounts, wusWalletName)
import           Pos.Util.Wlog (logDebug)
import           Pos.Wallet.Web.Account (GenSeed (..), genSaveRootKey,
                     genUniqueAccountId)
import           Pos.Wallet.Web.Backup (AccountMetaBackup (..),
                     WalletBackup (..), WalletMetaBackup (..))
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CAccountInit (..),
                     CAccountMeta (..), CBackupPhrase (..), CFilePath (..),
                     CId, CWallet (..), CWalletInit (..), CWalletMeta (..),
                     Wal, encToCId)
import           Pos.Wallet.Web.Error (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.State (AddressLookupMode (Ever), askWalletDB,
                     askWalletSnapshot, createAccount, getAccountWAddresses,
                     getWalletMeta, removeHistoryCache, setWalletSyncTip)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import qualified Pos.Wallet.Web.Tracking.Restore as Restore
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           UnliftIO (MonadUnliftIO)

-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = firstHardened

mnemonicExists :: Text
mnemonicExists = "Wallet with that mnemonics already exists"

-- | Creates a wallet. If the wallet with the given passphrase already exists,
-- then we return 'Left' of the wallet's ID.
mkWallet
    :: L.MonadWalletLogic ctx m
    => NetworkMagic
    -> PassPhrase
    -> CWalletInit
    -> Bool
    -> m (Either (CId Wal) (EncryptedSecretKey, CId Wal))
mkWallet nm passphrase CWalletInit {..} isReady = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase (bpToList cwBackupPhrase)
    let cAddr = encToCId nm skey

    eresult <- fmap Right (L.createWalletSafe nm cAddr cwInitMeta isReady)
        `catch` \(e :: WalletError) ->
            case e of
                RequestError msg | msg == mnemonicExists ->
                    pure (Left cAddr)
                _ ->
                    throwM e

    for eresult $ \CWallet{..} -> do

        -- can't return this result, since balances can change

        let accMeta = CAccountMeta { caName = "Initial account" }
            accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
        () <$ L.newAccountIncludeUnready nm True (DeterminedSeed initialAccAddrIdxs) passphrase accInit

        return (skey, cAddr)


newWallet
    :: L.MonadWalletLogic ctx m
    => NetworkMagic
    -> PassPhrase
    -> CWalletInit
    -> m CWallet
newWallet = throwMnemonicExists ... newWalletNoThrow

newWalletNoThrow
    :: L.MonadWalletLogic ctx m
    => NetworkMagic
    -> PassPhrase
    -> CWalletInit
    -> m (Either (CId Wal) CWallet)
newWalletNoThrow nm passphrase cwInit = do
    db <- askWalletDB
    -- A brand new wallet doesn't need any syncing, so we mark isReady=True
    eresult <- mkWallet nm passphrase cwInit True
    for eresult $ \(_, wId) -> do
        removeHistoryCache db wId
        -- BListener checks current syncTip before applying update,
        -- thus setting it up to date manually here
        withStateLockNoMetrics HighPriority $ \tip -> setWalletSyncTip db wId tip
        L.getWallet nm wId


{- | Restores a wallet from a seed. The process is conceptually divided into
-- two parts:
-- 1. Recover this wallet balance from the global Utxo (fast, and synchronous);
-- 2. Recover the full transaction history from the blockchain (slow, asynchronous).
-}
restoreWalletFromSeed
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Genesis.Config
    -> PassPhrase
    -> CWalletInit
    -> m CWallet
restoreWalletFromSeed = throwMnemonicExists ... restoreWalletFromSeedNoThrow

throwMnemonicExists :: MonadThrow m => m (Either (CId Wal) a) -> m a
throwMnemonicExists m = m >>= \case
    Left _ -> throwM (RequestError mnemonicExists)
    Right a -> pure a

-- | Restores a wallet without throwing an exception if the wallet already
-- exists.
restoreWalletFromSeedNoThrow
    :: ( L.MonadWalletLogic ctx m
      , MonadUnliftIO m
      , HasLens SyncQueue ctx SyncQueue
      )
    => Genesis.Config
    -> PassPhrase
    -> CWalletInit
    -> m (Either (CId Wal) CWallet)
restoreWalletFromSeedNoThrow genesisConfig passphrase cwInit = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    eresult <- mkWallet nm passphrase cwInit False
    for eresult $ \(sk, _) -> restoreWallet genesisConfig sk

restoreWallet :: ( L.MonadWalletLogic ctx m
                 , MonadUnliftIO m
                 , HasLens SyncQueue ctx SyncQueue
                 ) => Genesis.Config -> EncryptedSecretKey -> m CWallet
restoreWallet genesisConfig sk = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
        credentials@(_, wId) = eskToWalletDecrCredentials nm sk
    Restore.restoreWallet genesisConfig credentials
    db <- WS.askWalletDB
    WS.setWalletReady db wId True
    L.getWallet nm wId

restoreWalletFromBackup :: ( L.MonadWalletLogic ctx m
                           , MonadUnliftIO m
                           , HasLens SyncQueue ctx SyncQueue
                           ) => Genesis.Config -> WalletBackup -> m CWallet
restoreWalletFromBackup genesisConfig WalletBackup {..} = do
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    db <- askWalletDB
    ws <- getWalletSnapshot db
    let wId = encToCId nm wbSecretKey
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
                    () <$ L.newAccountIncludeUnready nm True defaultAccAddrIdx emptyPassphrase accInit
                else for_ accList $ \(idx, meta) -> do
                    let aIdx = fromInteger $ fromIntegral idx
                        seedGen = DeterminedSeed aIdx
                    accId <- genUniqueAccountId ws seedGen wId
                    createAccount db accId meta

            -- TODO(adn): Review the readyness story.
            void $ L.createWalletSafe nm wId wMeta False

            ws' <- askWalletSnapshot

            -- Get wallet accounts and create default address for each account
            -- without any existing address
            let wAccIds = getWalletAccountIds ws' wId
            for_ wAccIds $ \accId ->
                case getAccountWAddresses ws' Ever accId of
                    Nothing -> throwM $ InternalError "restoreWalletFromBackup: fatal: cannot find \
                                                      \an existing account of newly imported wallet"
                    Just [] -> void $ L.newAddress nm defaultAccAddrIdx emptyPassphrase accId
                    Just _  -> pure ()

            restoreWallet genesisConfig wbSecretKey

importWallet
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Genesis.Config
    -> PassPhrase
    -> CFilePath
    -> m CWallet
importWallet genesisConfig passphrase (CFilePath (toString -> fp)) = do
    secret <-
        rewrapToWalletError isDoesNotExistError noFile $
        rewrapToWalletError (\UserSecretDecodingError{} -> True) decodeFailed $
        readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWallet)
    importWalletDo genesisConfig passphrase wSecret
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
    => Genesis.Config
    -> PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletDo genesisConfig passphrase wSecret = do
    wId <- cwId <$> importWalletSecret genesisConfig emptyPassphrase wSecret
    let nm = makeNetworkMagic $ configProtocolMagic genesisConfig
    _ <- L.changeWalletPassphrase nm wId emptyPassphrase passphrase
    L.getWallet nm wId

importWalletSecret
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Genesis.Config
    -> PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret genesisConfig passphrase WalletUserSecret{..} = do
    let nm     = makeNetworkMagic $ configProtocolMagic genesisConfig
        key    = _wusRootKey
        wid    = encToCId nm key
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    -- TODO(adinapoli): Review the readiness story.
    void $ L.createWalletSafe nm wid wMeta False

    db <- askWalletDB
    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        ws <- askWalletSnapshot
        cAddr <- genUniqueAccountId ws seedGen wid
        createAccount db cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress nm (DeterminedSeed accountIndex) passphrase accId

    restoreWallet genesisConfig key

-- | Creates wallet with given genesis hd-wallet key.
-- For debug purposes
addInitialRichAccount
    :: ( L.MonadWalletLogic ctx m
       , MonadUnliftIO m
       , HasLens SyncQueue ctx SyncQueue
       )
    => Int
    -> Genesis.Config
    -> [PoorSecret]
    -> m ()
addInitialRichAccount keyId genesisConfig hdwSecretKeys =
    E.handleAny wSetExistsHandler $ do
        key <- maybeThrow noKey (map poorSecretToEncKey $ hdwSecretKeys ^? ix keyId)
        void $ importWalletSecret genesisConfig emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")
