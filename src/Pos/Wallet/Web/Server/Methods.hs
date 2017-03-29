{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods
       ( WalletWebHandler
       , walletApplication
       , walletServer
       , walletServeImpl
       , walletServerOuts
       ) where

import           Universum

import           Control.Concurrent            (forkFinally)
import           Control.Lens                  (makeLenses, (.=))
import           Control.Monad                 (replicateM_)
import           Control.Monad.Catch           (try)
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.State           (runStateT)
import           Data.Bits                     (setBit)
import qualified Data.ByteString.Base64        as B64
import           Data.Default                  (Default (def))
import           Data.List                     (elemIndex, (!!))
import           Data.Tagged                   (untag)
import qualified Data.Text                     as T
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Time.Units               (Microsecond, Second)
import           Formatting                    (build, ords, sformat, shown, stext, (%))
import           Network.Wai                   (Application)
import           Paths_cardano_sl              (version)
import           Pos.ReportServer.Report       (ReportType (RInfo))
import           Serokell.Util                 (threadDelay)
import           Servant.API                   ((:<|>) ((:<|>)),
                                                FromHttpApiData (parseUrlPiece))
import           Servant.Server                (Handler, Server, ServerT, err403, serve)
import           Servant.Utils.Enter           ((:~>) (..), enter)
import           System.Random                 (Random, randomIO)
import           System.Wlog                   (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes         ()
import           Pos.Client.Txp.History        (TxHistoryAnswer (..), TxHistoryEntry (..))
import           Pos.Communication             (OutSpecs, SendActions, hoistSendActions,
                                                sendTxOuts, submitRedemptionTx, submitTx)
import           Pos.Constants                 (curSoftwareVersion, isDevelopment)
import           Pos.Core                      (Address (..), Coin, addressF, coinF,
                                                createHDAddressH, decodeTextAddress,
                                                makePubKeyAddress, mkCoin, unsafeSubCoin)
import           Pos.Crypto                    (EncryptedSecretKey, PassPhrase,
                                                deriveHDPassphrase, encToPublic,
                                                fakeSigner, hash,
                                                redeemDeterministicKeyGen, toPublic,
                                                withSafeSigner, withSafeSigner)
import           Pos.DB.Limits                 (MonadDBLimits)
import           Pos.DHT.Model                 (getKnownPeers)
import           Pos.Reporting.MemState        (MonadReportingMem (..))
import           Pos.Reporting.Methods         (sendReportNodeNologs)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Txp.Core                  (TxOut (..), TxOutAux (..))
import           Pos.Util                      (maybeThrow)
import           Pos.Util.BackupPhrase         (BackupPhrase, safeKeysFromPhrase)
import           Pos.Util.UserSecret           (readUserSecret, usKeys)
import           Pos.Wallet.KeyStorage         (MonadKeys (..), addSecretKey)
import           Pos.Wallet.WalletMode         (WalletMode, applyLastUpdate,
                                                blockchainSlotDuration, connectedPeers,
                                                getBalance, getTxHistory,
                                                localChainDifficulty,
                                                networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (CAccount (..), CAccountAddress (..),
                                                CAddress, CCurrency (ADA), CInitialized,
                                                CPassPhrase (..), CProfile, CProfile (..),
                                                CTx (..), CTxId, CTxMeta (..),
                                                CUpdateInfo (..), CWallet (..),
                                                CWalletAddress (..), CWalletInit (..),
                                                CWalletMeta (..), CWalletRedeem (..),
                                                CWalletSet (..), CWalletSetAddress (..),
                                                CWalletSetInit (..), NotifyEvent (..),
                                                SyncProgress (..), addressToCAddress,
                                                cAddressToAddress,
                                                cPassPhraseToPassPhrase, mkCTx, mkCTxId,
                                                toCUpdateInfo, txContainsTitle,
                                                txIdToCTxId, walletAddrByAccount)
import           Pos.Wallet.Web.Error          (WalletError (..))
import           Pos.Wallet.Web.Server.Sockets (MonadWalletWebSockets (..),
                                                WalletWebSockets, closeWSConnection,
                                                getWalletWebSockets, initWSConnection,
                                                notify, runWalletWS, upgradeApplicationWS)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletWebDB,
                                                addAccount, addOnlyNewTxMeta, addUpdate,
                                                closeState, createWSet, createWallet,
                                                doesAccountExist, getHistoryCache,
                                                getNextUpdate, getProfile, getTxMeta,
                                                getWSetAddresses, getWSetMeta,
                                                getWalletAccounts, getWalletAddresses,
                                                getWalletMeta, getWalletState, openState,
                                                removeAccount, removeNextUpdate,
                                                removeWallet, runWalletWebDB, setProfile,
                                                setWalletMeta, setWalletTransactionMeta,
                                                testReset, updateHistoryCache)
import           Pos.Web.Server                (serveImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type WalletWebHandler m = WalletWebSockets (WalletWebDB m)

type WalletWebMode ssc m
    = ( WalletMode ssc m
      , MonadWalletWebDB m
      , MonadDBLimits m
      , MonadWalletWebSockets m
      , MonadReportingMem m
      )

makeLenses ''SyncProgress

walletServeImpl
    :: ( MonadIO m
       , MonadMask m
       , WalletWebMode ssc (WalletWebHandler m))
    => WalletWebHandler m Application     -- ^ Application getter
    -> FilePath                           -- ^ Path to wallet acid-state
    -> Bool                               -- ^ Rebuild flag for acid-state
    -> Word16                             -- ^ Port to listen
    -> m ()
walletServeImpl app daedalusDbPath dbRebuild port =
    bracket pre post $ \(db, conn) ->
        serveImpl (runWalletWebDB db $ runWalletWS conn app) "127.0.0.1" port
  where
    pre = (,) <$> openDB <*> initWS
    post (db, conn) = closeDB db >> closeWS conn
    openDB = openState dbRebuild daedalusDbPath
    closeDB = closeState
    initWS = putText "walletServeImpl initWsConnection" >> initWSConnection
    closeWS = closeWSConnection

walletApplication
    :: WalletWebMode ssc m
    => m (Server WalletApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    serv >>= return . upgradeApplicationWS wsConn . serve walletApi

walletServer
    :: forall ssc m.
       (SscHelpersClass ssc, Monad m, MonadIO m, WalletWebMode ssc (WalletWebHandler m))
    => SendActions m
    -> WalletWebHandler m (WalletWebHandler m :~> Handler)
    -> WalletWebHandler m (Server WalletApi)
walletServer sendActions nat = do
    whenM (isNothing <$> getProfile) $
        createUserProfile >>= setProfile
    ws    <- lift getWalletState
    socks <- getWalletWebSockets
    let sendActions' = hoistSendActions
            (lift . lift)
            (runWalletWebDB ws . runWalletWS socks)
            sendActions
    nat >>= launchNotifier
    myRootAddresses >>= mapM_ insertAddressMeta
    (`enter` servantHandlers @ssc sendActions') <$> nat
  where
    insertAddressMeta cAddr = do
        getWSetMeta cAddr >>= createWSet cAddr . fromMaybe def
    createUserProfile = do
        time <- liftIO getPOSIXTime
        pure $ CProfile mempty mempty mempty mempty time mempty mempty

----------------------------------------------------------------------------
-- Notifier
----------------------------------------------------------------------------

-- FIXME: this is really inaficient. Temporary solution
launchNotifier :: WalletWebMode ssc m => (m :~> Handler) -> m ()
launchNotifier nat =
    void . liftIO $ mapM startForking
        [ dificultyNotifier
        , updateNotifier
        ]
  where
    cooldownPeriod :: Second
    cooldownPeriod = 5

    difficultyNotifyPeriod :: Microsecond
    difficultyNotifyPeriod = 500000  -- 0.5 sec

    -- networkResendPeriod = 10         -- in delay periods
    forkForever action = forkFinally action $ const $ do
        -- TODO: log error
        -- cooldown
        threadDelay cooldownPeriod
        void $ forkForever action
    -- TODO: use Servant.enter here
    -- FIXME: don't ignore errors, send error msg to the socket
    startForking = forkForever . void . runExceptT . unNat nat
    notifier period action = forever $ do
        liftIO $ threadDelay period
        action
    dificultyNotifier = void . flip runStateT def $ notifier difficultyNotifyPeriod $ do
        whenJustM networkChainDifficulty $
            \networkDifficulty -> do
                oldNetworkDifficulty <- use spNetworkCD
                when (Just networkDifficulty /= oldNetworkDifficulty) $ do
                    lift $ notify $ NetworkDifficultyChanged networkDifficulty
                    spNetworkCD .= Just networkDifficulty

        localDifficulty <- localChainDifficulty
        oldLocalDifficulty <- use spLocalCD
        when (localDifficulty /= oldLocalDifficulty) $ do
            lift $ notify $ LocalDifficultyChanged localDifficulty
            spLocalCD .= localDifficulty

        peers <- connectedPeers
        oldPeers <- use spPeers
        when (peers /= oldPeers) $ do
            lift $ notify $ ConnectedPeersChanged peers
            spPeers .= peers

    updateNotifier = do
        cps <- waitForUpdate
        addUpdate $ toCUpdateInfo cps
        logDebug "Added update to wallet storage"
        notify UpdateAvailable

    -- historyNotifier :: WalletWebMode ssc m => m ()
    -- historyNotifier = do
    --     cAddresses <- addressToCAddress <<$>> myRootAddresses
    --     forM_ cAddresses $ \cAddress -> do
    --         -- TODO: is reading from acid RAM only (not reading from disk?)
    --         oldHistoryLength <- length . fromMaybe mempty <$> getWalletHistory cAddress
    --         newHistoryLength <- length <$> getHistory cAddress
    --         when (oldHistoryLength /= newHistoryLength) .
    --             notify $ NewWalletTransaction cAddress

walletServerOuts :: OutSpecs
walletServerOuts = sendTxOuts

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => SendActions m -> ServerT WalletApi m
servantHandlers sendActions =
     catchWalletError testResetAll
    :<|>

     apiGetWSet
    :<|>
     apiGetWSets
    :<|>
     apiNewWSet
    :<|>
     apiRestoreWSet
    :<|>
     apiImportKey
    :<|>

     apiGetWallet
    :<|>
     apiGetWallets
    :<|>
     apiUpdateWallet
    :<|>
     apiNewWallet
    :<|>
     apiDeleteWallet
    :<|>

     apiNewAccount
    :<|>

     apiIsValidAddress
    :<|>

     apiGetUserProfile
    :<|>
     apiUpdateUserProfile
    :<|>

     apiTxsPayments
    :<|>
     apiTxsPaymentsExt
    :<|>
     apiUpdateTransaction
    :<|>
     apiGetHistory
    :<|>
     apiSearchHistory
    :<|>

     apiNextUpdate
    :<|>
     apiApplyUpdate
    :<|>

     apiRedeemAda
    :<|>

     apiReportingInitialized
    :<|>

     apiSettingsSlotDuration
    :<|>
     apiSettingsSoftwareVersion
    :<|>
     apiSettingsSyncProgress
  where
    -- TODO: can we with Traversable map catchWalletError over :<|>
    -- TODO: add logging on error
    apiGetWSet                  = (catchWalletError . getWSet)
    apiGetWSets                 = catchWalletError getWSets
    apiNewWSet                  = (\a -> catchWalletError . newWSet a)
    apiRestoreWSet              = (\a -> catchWalletError . restoreWSet a)
    apiImportKey                = catchWalletError . importKey sendActions
    apiGetWallet                = catchWalletError . getWallet
    apiGetWallets               = catchWalletError . getWallets
    apiUpdateWallet             = (\a -> catchWalletError . updateWallet a)
    apiNewWallet                = (\a -> catchWalletError . newWallet a)
    apiDeleteWallet             = catchWalletError . deleteWallet
    apiNewAccount               = (\a -> catchWalletError . newAccount a)
    apiIsValidAddress           = (\a -> catchWalletError . isValidAddress a)
    apiGetUserProfile           = catchWalletError getUserProfile
    apiUpdateUserProfile        = catchWalletError . updateUserProfile
    apiTxsPayments              = (\a b c -> catchWalletError . send sendActions a b c)
    apiTxsPaymentsExt           = (\a b c d e f -> catchWalletError . sendExtended sendActions a b c d e f)
    apiUpdateTransaction        = (\a b -> catchWalletError . updateTransaction a b)
    apiGetHistory               = (\a b -> catchWalletError . getHistory @ssc a b )
    apiSearchHistory            = (\a b c d -> catchWalletError . searchHistory @ssc a b c d)
    apiNextUpdate               = catchWalletError nextUpdate
    apiApplyUpdate              = catchWalletError applyUpdate
    apiRedeemAda                = (\a -> catchWalletError . redeemADA sendActions a)
    apiReportingInitialized     = catchWalletError . reportingInitialized
    apiSettingsSlotDuration     = catchWalletError (fromIntegral <$> blockchainSlotDuration)
    apiSettingsSoftwareVersion  = catchWalletError (pure curSoftwareVersion)
    apiSettingsSyncProgress     = catchWalletError syncProgress

    catchWalletError            = try

getUserProfile :: WalletWebMode ssc m => m CProfile
getUserProfile = getProfile >>= maybe noProfile pure
  where
    noProfile = throwM $ Internal "No user profile"

updateUserProfile :: WalletWebMode ssc m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getAccountBalance :: WalletWebMode ssc m => CAccountAddress -> m Coin
getAccountBalance cAccAddr =
    getBalance <=< decodeCAddressOrFail $ caaAddress cAccAddr

getAccount :: WalletWebMode ssc m => CAccountAddress -> m CAccount
getAccount cAddr = do
    balance <- getAccountBalance cAddr
    return $ CAccount cAddr balance

getWalletAccAddrsOrThrow :: WalletWebMode ssc m => CWalletAddress -> m [CAccountAddress]
getWalletAccAddrsOrThrow wCAddr =
    getWalletAccounts wCAddr >>= maybe noWallet return
  where
    noWallet = throwM . Internal $
        sformat ("No wallet with address "%build%" is found") wCAddr

getAccounts :: WalletWebMode ssc m => CWalletAddress -> m [CAccount]
getAccounts = getWalletAccAddrsOrThrow >=> mapM getAccount

getWallet :: WalletWebMode ssc m => CWalletAddress -> m CWallet
getWallet cAddr = do
    accounts <- getAccounts cAddr
    meta     <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr meta accounts
  where
    noWallet = throwM . Internal $
        sformat ("No wallet with address "%build%" is found") cAddr

getWSet :: WalletWebMode ssc m => CWalletSetAddress -> m CWalletSet
getWSet cAddr = do
    meta       <- getWSetMeta cAddr >>= maybe noWSet pure
    walletsNum <- length <$> getWallets (Just cAddr)
    pure $ CWalletSet cAddr meta walletsNum
  where
    noWSet = throwM . Internal $
        sformat ("No wallet set with address "%build%" is found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: WalletWebMode ssc m => CAddress -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode ssc m => Maybe CWalletSetAddress -> m [CWallet]
getWallets mCAddr = mapM getWallet . filterByAddr mCAddr =<< getWalletAddresses
  where
    filterByAddr = maybe identity $ \cAddr ->
        filter $ (== cAddr) . cwaWSAddress

getWSets :: WalletWebMode ssc m => m [CWalletSet]
getWSets = getWSetAddresses >>= mapM getWSet

decodeCPassPhraseOrFail :: WalletWebMode ssc m => CPassPhrase -> m PassPhrase
decodeCPassPhraseOrFail cpass =
    either (const . throwM $ Internal "Decoding of passphrase failed") return $
    cPassPhraseToPassPhrase cpass

send :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CAccountAddress -> CAccountAddress -> Coin -> m CTx
send sendActions cpass srcCAddr dstCAddr c =
    sendExtended sendActions cpass srcCAddr dstCAddr c ADA mempty mempty

sendExtended :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CAccountAddress -> CAccountAddress -> Coin -> CCurrency -> Text -> Text -> m CTx
sendExtended sendActions cpassphrase srcCAddr dstCAddr c curr title desc = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    let srcWCAddr = walletAddrByAccount srcCAddr
    dstAddr   <- decodeCAddressOrFail $ caaAddress dstCAddr
    (idx, sk) <- getSKByAddr $ caaWSAddress srcCAddr
    let mainTx = TxOutAux (TxOut dstAddr c) []
    balance <- getAccountBalance srcCAddr
    mRems <- if balance < c
        then return Nothing
        else do
            remCAddr <- genUniqueAccountAddress passphrase srcWCAddr
            remAddr  <- decodeCAddressOrFail $ caaAddress remCAddr
            let remTx = TxOutAux (TxOut remAddr $ balance `unsafeSubCoin` c) []
            return $ Just (remTx, remCAddr)
    na <- getKnownPeers
    withSafeSigner sk (return passphrase) $ \ss -> do
        etx <- submitTx sendActions ss na (mainTx :| maybe mempty (one . fst) mRems)
        case etx of
            Left err -> throwM . Internal $ sformat ("Cannot send transaction: "%stext) err
            Right (tx, _, _) -> do
                logInfo $
                    sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
                    c idx dstAddr
                -- TODO: this should be removed in production
                let txHash = hash tx
                whenJust mRems $ \(_, remCAddr) -> do
                    () <$ addHistoryTx remCAddr curr title desc (THEntry txHash tx True Nothing)
                    removeAccount srcCAddr
                () <$ addHistoryTx dstCAddr curr title desc (THEntry txHash tx False Nothing)
                addHistoryTx srcCAddr curr title desc (THEntry txHash tx True Nothing)

getHistory
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => CWalletAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory wAddr skip limit = do
    cAccAddrs <- getWalletAccAddrsOrThrow wAddr
    cHistory <- fmap concat . forM cAccAddrs $ \cAccAddr -> do
        (minit, cachedTxs) <- transCache <$> getHistoryCache cAccAddr
        cAddr <- decodeCAddressOrFail $ caaAddress cAccAddr

        TxHistoryAnswer {..} <- untag @ssc getTxHistory cAddr minit

        -- Add allowed portion of result to cache
        let fullHistory = taHistory <> cachedTxs
            lenHistory = length taHistory
            cached = drop (lenHistory - taCachedNum) taHistory

        unless (null cached) $
            updateHistoryCache cAccAddr taLastCachedHash taCachedUtxo
            (cached <> cachedTxs)

        mapM (addHistoryTx cAccAddr ADA mempty mempty) fullHistory
    pure (paginate cHistory, fromIntegral $ length cHistory)
  where
    paginate     = take defaultLimit . drop defaultSkip
    defaultLimit = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip  = (fromIntegral $ fromMaybe 0 skip)
    transCache Nothing                = (Nothing, [])
    transCache (Just (hh, utxo, txs)) = (Just (hh, utxo), txs)

-- FIXME: is Word enough for length here?
searchHistory
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => CWalletAddress -> Text -> Maybe CAccountAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
searchHistory wAddr search mAccAddr skip limit =
    first (filter fits) <$> getHistory @ssc wAddr skip limit
  where
    fits ctx = txContainsTitle search ctx
            && maybe (const True) (==) mAccAddr (ctAccAddr ctx)

addHistoryTx
    :: WalletWebMode ssc m
    => CAccountAddress
    -> CCurrency
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cAddr curr title desc wtx@(THEntry txId _ _ _) = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    addr <- decodeCAddressOrFail . cwsaAddress $ caaWSAddress cAddr
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId txId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- maybe meta identity <$> getTxMeta cAddr cId
    return $ mkCTx addr diff wtx meta' cAddr

newAccount :: WalletWebMode ssc m => CPassPhrase -> CWalletAddress -> m CAccount
newAccount cPassphrase cWAddr = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAccAddr <- genUniqueAccountAddress passphrase cWAddr
    addAccount cAccAddr
    getAccount cAccAddr

newWallet :: WalletWebMode ssc m => CPassPhrase -> CWalletInit -> m CWallet
newWallet cPassphrase CWalletInit {..} = do
    cAddr <- genUniqueWalletAddress cwInitWSetId
    createWallet cAddr cwInitMeta
    () <$ newAccount cPassphrase cAddr
    getWallet cAddr

newWSet :: WalletWebMode ssc m => CPassPhrase -> CWalletSetInit -> m CWalletSet
newWSet cPassphrase CWalletSetInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAddr <- genSaveRootAddress passphrase cwsBackupPhrase
    createWSet cAddr cwsInitMeta
    getWSet cAddr

restoreWSet :: WalletWebMode ssc m => CPassPhrase -> CWalletSetInit -> m CWalletSet
restoreWSet cPassphrase CWalletSetInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAddr <- genSaveRootAddress passphrase cwsBackupPhrase
    getWSetMeta cAddr
        >>= maybe (createWSet cAddr cwsInitMeta) (const wSetExistsError)
    getWSet cAddr
  where
    wSetExistsError = throwM $ Internal "Wallet set with that mnemonics already exists"

updateWallet :: WalletWebMode ssc m => CWalletAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode ssc m => CAccountAddress -> CTxId -> CTxMeta -> m ()
updateTransaction cAddr = setWalletTransactionMeta cAddr

deleteWallet :: WalletWebMode ssc m => CWalletAddress -> m ()
deleteWallet = removeWallet

{- TODO: uncomment once required by spec
deleteWSet :: WalletWebMode ssc m => CWalletSetAddress -> m ()
deleteWSet cAddr = do
    deleteAddress =<< decodeCAddressOrFail cAddr
    removeWSet cAddr
  where
    deleteAddress addr = do
        idx <- getAddrIdx addr
        deleteSecretKey (fromIntegral idx) `catch` deleteErrHandler
    deleteErrHandler (PrimaryKey err) = throwM . Internal $
        sformat ("Error while deleting wallet set: "%stext) err
-}

addressToWSAddress :: Address -> CWalletSetAddress
addressToWSAddress = CWalletSetAddress . addressToCAddress

-- NOTE: later we will have `isValidAddress :: CCurrency -> CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode ssc m => Text -> CCurrency -> m Bool
isValidAddress sAddr ADA = pure . either (const False) (const True) $ decodeTextAddress sAddr
isValidAddress _ _       = pure False

-- | Get last update info
nextUpdate :: WalletWebMode ssc m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (Internal "No updates available")

applyUpdate :: WalletWebMode ssc m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemADA :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CWalletRedeem -> m CTx
redeemADA sendActions cpassphrase CWalletRedeem {..} = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    seedBs <- either
        (\e -> throwM $ Internal ("Seed is invalid base64 string: " <> toText e))
        pure $ B64.decode (encodeUtf8 crSeed)
    (_, redeemSK) <- maybeThrow (Internal "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs

    dstCAddr <- genUniqueAccountAddress passphrase crWalletId
    dstAddr <- decodeCAddressOrFail $ caaAddress dstCAddr
    na <- getKnownPeers
    etx <- submitRedemptionTx sendActions redeemSK na dstAddr
    case etx of
        Left err -> throwM . Internal $ "Cannot send redemption transaction: " <> err
        Right (tx, _, _) -> do
            -- add redemption transaction to the history of new wallet
            addHistoryTx dstCAddr ADA "ADA redemption" ""
              (THEntry (hash tx) tx False Nothing)

reportingInitialized :: forall ssc m. WalletWebMode ssc m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs version (RInfo $ show cinit) `catch` handler
  where
    handler :: SomeException -> m ()
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

importKey
    :: WalletWebMode ssc m
    => SendActions m
    -> Text
    -> m CWalletSet
importKey sendActions (toString -> fp) = do
    secret <- readUserSecret fp
    let keys = secret ^. usKeys
    forM_ keys $ \key -> do
        addSecretKey key
        let addr = makePubKeyAddress $ encToPublic key
            wsAddr = addressToWSAddress addr
        createWSet wsAddr def

    let importedAddr = makePubKeyAddress $ encToPublic (keys !! 0)
        importedWSAddr = addressToWSAddress importedAddr
    accountCAddr <- undefined importedAddr

    -- TODO [CSL-931]: What is supposed to be here?
    -- We can't just send money from wallet set to wallet set
    when isDevelopment $ do
        psk <- maybeThrow (Internal "No primary key is present!")
               =<< getPrimaryKey
        let pAddr = makePubKeyAddress $ toPublic psk
        primaryBalance <- getBalance pAddr
        when (primaryBalance > mkCoin 0) $ do
            na <- getKnownPeers
            etx <- submitTx sendActions (fakeSigner psk) na
                       (one $ TxOutAux (TxOut importedAddr primaryBalance) [])
            -- TODO [CSL-931]: generate wallet and account along with wallet set
            -- and send to it, not to master key
            case etx of
                Left err -> throwM . Internal $ "Cannot transfer funds from genesis key" <> err
                Right (tx, _, _) -> void $
                    addHistoryTx accountCAddr ADA "Transfer money from genesis key" ""
                        (THEntry (hash tx) tx False Nothing)
    getWSet importedWSAddr

syncProgress :: WalletWebMode ssc m => m SyncProgress
syncProgress = do
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

testResetAll :: WalletWebMode ssc m => m ()
testResetAll | isDevelopment = deleteAllKeys >> testReset
             | otherwise     = throwM err403
  where
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0

---------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myRootAddresses :: MonadKeys m => m [CWalletSetAddress]
myRootAddresses =
    addressToWSAddress . makePubKeyAddress . encToPublic <<$>> getSecretKeys

getAddrIdx :: WalletWebMode ssc m => CWalletSetAddress -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybe notFound pure
  where notFound = throwM . Internal $
            sformat ("Address "%build%" is not found in wallet") addr

getSKByAddr :: WalletWebMode ssc m => CWalletSetAddress -> m (Int, EncryptedSecretKey)
getSKByAddr cAddr = do
    idx  <- getAddrIdx cAddr
    sks  <- getSecretKeys
    let sk = sks !! idx
    return (idx, sk)

genSaveRootAddress
    :: WalletWebMode ssc m
    => PassPhrase
    -> BackupPhrase
    -> m CWalletSetAddress
genSaveRootAddress passphrase ph =
    addressToWSAddress . makePubKeyAddress . encToPublic <$> genSaveSK
  where
    genSaveSK = do
        let sk = fst $ safeKeysFromPhrase passphrase ph
        addSecretKey sk
        return sk

generateUnique :: (MonadIO m, Random a) => (a -> m b) -> (b -> m Bool) -> m b
generateUnique generator isDuplicate = loop
  where
    loop = do
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- isDuplicate value
        if bad
            then loop
            else return value

nonHardenedOnly :: Word32 -> Word32
nonHardenedOnly index = setBit index 31

genUniqueWalletAddress
    :: WalletWebMode ssc m
    => CWalletSetAddress
    -> m CWalletAddress
genUniqueWalletAddress wsCAddr =
    generateUnique (return . CWalletAddress wsCAddr . nonHardenedOnly)
                   (fmap isJust . getWalletMeta)

genUniqueAccountAddress
    :: WalletWebMode ssc m
    => PassPhrase
    -> CWalletAddress
    -> m CAccountAddress
genUniqueAccountAddress passphrase wCAddr@CWalletAddress{..} =
    generateUnique (mkAccount . nonHardenedOnly)
                    doesAccountExist
  where
    mkAccount caaAccountIndex = do
        (address, _) <- deriveAccountSK passphrase wCAddr caaAccountIndex
        let caaWSAddress   = cwaWSAddress
            caaWalletIndex = cwaIndex
            caaAddress     = addressToCAddress address
        return CAccountAddress{..}

deriveAccountSK
    :: WalletWebMode ssc m
    => PassPhrase
    -> CWalletAddress
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountSK passphrase CWalletAddress{..} accIndex = do
    (_, wSetSk) <- getSKByAddr cwaWSAddress
    let hdPass   = deriveHDPassphrase $ encToPublic wSetSk
    let derive   = createHDAddressH passphrase hdPass
    let (_, wSk) = derive wSetSk [] cwaIndex
    return $ derive wSk [cwaIndex] accIndex

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress

instance FromHttpApiData CWalletSetAddress where
    parseUrlPiece = fmap CWalletSetAddress . parseUrlPiece

instance FromHttpApiData CWalletAddress where
    parseUrlPiece url =
        case T.splitOn "#" url of
            [part1, part2] -> do
                cwaWSAddress <- parseUrlPiece part1
                cwaIndex     <- maybe (Left "Invalid wallet index") Right $
                                readMaybe $ toString part2
                return CWalletAddress{..}
            _ -> Left "Expected 2 parts separated by '#'"

instance FromHttpApiData CAccountAddress where
    parseUrlPiece url =
        case T.splitOn "#" url of
            [part1, part2, part3, part4] -> do
                caaWSAddress    <- parseUrlPiece part1
                caaWalletIndex  <- maybe (Left "Invalid wallet index") Right $
                                   readMaybe $ toString part2
                caaAccountIndex <- maybe (Left "Invalid account index") Right $
                                   readMaybe $ toString part3
                caaAddress      <- parseUrlPiece part4
                return CAccountAddress{..}
            _ -> Left "Expected 4 parts separated by '#'"

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking is receaved Text really vald CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CCurrency where
    parseUrlPiece = first fromString . readEither . toString

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
