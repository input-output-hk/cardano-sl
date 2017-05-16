{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Control.Lens                  (ix, makeLenses, (.=))
import           Control.Monad.Catch           (SomeException, catches, try)
import qualified Control.Monad.Catch           as E
import           Control.Monad.State           (runStateT)
import           Data.Default                  (Default (def))
import           Data.List                     (elemIndex, (!!))
import qualified Data.List.NonEmpty            as NE
import           Data.Tagged                   (untag)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Time.Units               (Microsecond, Second)
import qualified Ether
import           Formatting                    (build, ords, sformat, shown, stext, (%))
import           Network.Wai                   (Application)
import           Paths_cardano_sl              (version)
import           Pos.ReportServer.Report       (ReportType (RInfo))
import           Serokell.Util                 (threadDelay)
import qualified Serokell.Util.Base64          as B64
import           Servant.API                   ((:<|>) ((:<|>)),
                                                FromHttpApiData (parseUrlPiece))
import           Servant.Multipart             (fdFilePath)
import           Servant.Server                (Handler, Server, ServerT, err403,
                                                runHandler, serve)
import           Servant.Utils.Enter           ((:~>) (..), enter)
import           System.Wlog                   (logDebug, logError, logInfo)

import           Data.ByteString.Base58        (bitcoinAlphabet, decodeBase58)

import           Pos.Aeson.ClientTypes         ()
import           Pos.Client.Txp.History        (TxHistoryAnswer (..), TxHistoryEntry (..))
import           Pos.Communication             (OutSpecs, SendActions, hoistSendActions,
                                                sendTxOuts, submitRedemptionTx, submitTx)
import           Pos.Constants                 (curSoftwareVersion, isDevelopment)
import           Pos.Core                      (Address, Coin, addressF, coinF,
                                                decodeTextAddress, makePubKeyAddress,
                                                mkCoin)
import           Pos.Crypto                    (PassPhrase, aesDecrypt, deriveAesKeyBS,
                                                encToPublic, hash,
                                                redeemDeterministicKeyGen, withSafeSigner,
                                                withSafeSigner)
import           Pos.DB.Class                  (MonadGStateCore)
import           Pos.Discovery                 (getPeers)
import           Pos.Reporting.MemState        (MonadReportingMem, rcReportServers)
import           Pos.Reporting.Methods         (sendReport, sendReportNodeNologs)
import           Pos.Txp.Core                  (TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Util                      (maybeThrow)
import           Pos.Util.BackupPhrase         (BackupPhrase, safeKeysFromPhrase, toSeed)
import           Pos.Util.UserSecret           (readUserSecret, usKeys)
import           Pos.Wallet.KeyStorage         (KeyError (..), MonadKeys, addSecretKey,
                                                addSecretKey, deleteSecretKey,
                                                getSecretKeys)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.WalletMode         (MonadBlockchainInfo, MonadTxHistory,
                                                WalletMode, applyLastUpdate,
                                                blockchainSlotDuration, connectedPeers,
                                                getBalance, getTxHistory,
                                                localChainDifficulty,
                                                networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (CAddress, CCurrency (ADA),
                                                CElectronCrashReport (..), CInitialized,
                                                CPaperVendWalletRedeem (..),
                                                CPassPhrase (..), CProfile, CProfile (..),
                                                CTx, CTxId, CTxMeta (..),
                                                CUpdateInfo (..), CWallet (..),
                                                CWalletInit (..), CWalletMeta (..),
                                                CWalletRedeem (..), NotifyEvent (..),
                                                SyncProgress (..), addressToCAddress,
                                                cAddressToAddress,
                                                cPassPhraseToPassPhrase, mkCCoin, mkCTx,
                                                mkCTxId, toCUpdateInfo, txContainsTitle,
                                                txIdToCTxId)
import           Pos.Wallet.Web.Error          (WalletError (..))
import           Pos.Wallet.Web.Server.Sockets (MonadWalletWebSockets, WalletWebSockets,
                                                closeWSConnection, getWalletWebSockets,
                                                getWalletWebSockets, initWSConnection,
                                                notify, runWalletWS, upgradeApplicationWS)
import           Pos.Wallet.Web.State          (WalletWebDB, WebWalletModeDB,
                                                addOnlyNewTxMeta, addUpdate, closeState,
                                                createWallet, getHistoryCache,
                                                getNextUpdate, getProfile, getTxMeta,
                                                getWalletMeta, getWalletState, openState,
                                                removeNextUpdate, removeWallet,
                                                runWalletWebDB, setProfile, setWalletMeta,
                                                setWalletTransactionMeta, testReset,
                                                updateHistoryCache)
import           Pos.Web.Server                (serveImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type WalletWebHandler m = WalletWebSockets (WalletWebDB m)

type WalletWebMode m
    = ( WalletMode WalletSscType m
      , WebWalletModeDB m
      , MonadGStateCore m
      , MonadWalletWebSockets m
      , MonadReportingMem m
      )

makeLenses ''SyncProgress

walletServeImpl
    :: ( MonadIO m
       , MonadMask m
       , WalletWebMode (WalletWebHandler m))
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
    :: WalletWebMode m
    => m (Server WalletApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS wsConn . serve walletApi <$> serv

walletServer
    :: (Monad m, MonadIO m, WalletWebMode (WalletWebHandler m))
    => SendActions m
    -> WalletWebHandler m (WalletWebHandler m :~> Handler)
    -> WalletWebHandler m (Server WalletApi)
walletServer sendActions nat = do
    ws    <- lift getWalletState
    socks <- getWalletWebSockets
    let sendActions' = hoistSendActions
            (lift . lift)
            (runWalletWebDB ws . runWalletWS socks)
            sendActions
    nat >>= launchNotifier
    myCAddresses >>= mapM_ insertAddressMeta
    (`enter` servantHandlers sendActions') <$> nat
  where
    insertAddressMeta cAddr =
        getWalletMeta cAddr >>= createWallet cAddr . fromMaybe def

----------------------------------------------------------------------------
-- Notifier
----------------------------------------------------------------------------

-- FIXME: this is really inefficient. Temporary solution
launchNotifier :: WalletWebMode m => (m :~> Handler) -> m ()
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
    startForking = forkForever . void . runHandler . ($$) nat
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
    --     cAddresses <- myCAddresses
    --     for_ cAddresses $ \cAddress -> do
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
    :: WalletWebMode m
    => SendActions m
    -> ServerT WalletApi m
servantHandlers sendActions =
     catchWalletError testResetAll
    :<|>
     apiGetWallet
    :<|>
     apiGetWallets
    :<|>
     apiUpdateWallet
    :<|>
     apiDeleteWallet
    :<|>
     apiImportKey
    :<|>
     apiRestoreWallet
    :<|>
     apiNewWallet
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
     apiRedeemAdaPaperVend
    :<|>
     apiReportingInitialized
    :<|>
     apiReportingElectroncrash
    :<|>
     apiSettingsSlotDuration
    :<|>
     apiSettingsSoftwareVersion
    :<|>
     apiSettingsSyncProgress
  where
    -- TODO: can we with Traversable map catchWalletError over :<|>
    -- TODO: add logging on error
    apiGetWallet                = (catchWalletError . getWallet)
    apiGetWallets               = catchWalletError getWallets
    apiUpdateWallet             = (\a -> catchWalletError . updateWallet a)
    apiNewWallet                = (\a -> catchWalletError . newWallet a)
    apiDeleteWallet             = catchWalletError . deleteWallet
    apiImportKey                = (catchWalletError . importKey)
    apiRestoreWallet            = (\a -> catchWalletError . restoreWallet a)
    apiIsValidAddress           = (\a -> catchWalletError . isValidAddress a)
    apiGetUserProfile           = catchWalletError getUserProfile
    apiUpdateUserProfile        = catchWalletError . updateUserProfile
    apiTxsPayments              = (\a b c -> catchWalletError . send sendActions a b c)
    apiTxsPaymentsExt           = (\a b c d e f -> catchWalletError . sendExtended sendActions a b c d e f)
    apiUpdateTransaction        = (\a b -> catchWalletError . updateTransaction a b)
    apiGetHistory               = (\a b -> catchWalletError . getHistory a b)
    apiSearchHistory            = (\a b c -> catchWalletError . searchHistory a b c)
    apiNextUpdate               = catchWalletError nextUpdate
    apiApplyUpdate              = catchWalletError applyUpdate
    apiRedeemAda                = catchWalletError . redeemAda sendActions
    apiRedeemAdaPaperVend       = catchWalletError . redeemAdaPaperVend sendActions
    apiReportingInitialized     = catchWalletError . reportingInitialized
    apiReportingElectroncrash   = catchWalletError . reportingElectroncrash
    apiSettingsSlotDuration     = catchWalletError (fromIntegral <$> blockchainSlotDuration)
    apiSettingsSoftwareVersion  = catchWalletError (pure curSoftwareVersion)
    apiSettingsSyncProgress     = catchWalletError syncProgress

    catchWalletError            = try

-- getAddresses :: WalletWebMode ssc m => m [CAddress]
-- getAddresses = map addressToCAddress <$> myAddresses

-- getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getUserProfile :: WalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: WalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getWallet :: WalletWebMode m => CAddress -> m CWallet
getWallet cAddr = do
    balance <- fmap mkCCoin . getBalance =<< decodeCAddressOrFail cAddr
    meta <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr balance meta
  where
    noWallet = throwM . Internal $
        sformat ("No wallet with address "%build%" is found") cAddr

decodeCAddressOrFail :: MonadThrow m => CAddress -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode m => m [CWallet]
getWallets = join $ mapM getWallet <$> myCAddresses

send
    :: WalletWebMode m
    => SendActions m
    -> CPassPhrase
    -> CAddress
    -> CAddress
    -> Coin
    -> m CTx
send sendActions cpass srcCAddr dstCAddr c =
    sendExtended sendActions cpass srcCAddr dstCAddr c ADA mempty mempty

sendExtended
    :: WalletWebMode m
    => SendActions m
    -> CPassPhrase
    -> CAddress
    -> CAddress
    -> Coin
    -> CCurrency
    -> Text
    -> Text
    -> m CTx
sendExtended sendActions cpassphrase srcCAddr dstCAddr c curr title desc = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    srcAddr <- decodeCAddressOrFail srcCAddr
    dstAddr <- decodeCAddressOrFail dstCAddr
    idx <- getAddrIdx srcAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    na <- getPeers
    withSafeSigner sk (return passphrase) $ \mss -> do
        ss  <- mss `whenNothing` throwM (Internal "Passphrase doesn't match")
        etx <- submitTx sendActions ss (toList na) (one $ TxOutAux (TxOut dstAddr c) [])
        case etx of
            Left err -> throwM . Internal $ sformat ("Cannot send transaction: "%stext) err
            Right (TxAux {taTx = tx}) -> do
                logInfo $
                    sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
                    c idx dstAddr
                -- TODO: this should be removed in production
                let txHash = hash tx
                () <$ addHistoryTx dstCAddr curr title desc (THEntry txHash tx False [TxOut srcAddr c] Nothing)
                addHistoryTx srcCAddr curr title desc (THEntry txHash tx True [TxOut srcAddr c] Nothing)

getHistory
    :: (WebWalletModeDB m, MonadTxHistory m, MonadThrow m, MonadBlockchainInfo m)
    => CAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory cAddr skip limit = do
    (minit, cachedTxs) <- transCache <$> getHistoryCache cAddr

    -- TODO: Fix type param! Global type param.
    TxHistoryAnswer {..} <- flip (untag @WalletSscType getTxHistory) minit
        =<< decodeCAddressOrFail cAddr

    -- Add allowed portion of result to cache
    let fullHistory = taHistory <> cachedTxs
        lenHistory  = length taHistory
        cached      = drop (lenHistory - taCachedNum) taHistory

    unless (null cached) $
        updateHistoryCache cAddr taLastCachedHash taCachedUtxo (cached <> cachedTxs)

    cHistory <- mapM (addHistoryTx cAddr ADA mempty mempty) fullHistory
    pure (paginate cHistory, fromIntegral $ length cHistory)
  where
    paginate                            = take defaultLimit . drop defaultSkip
    defaultLimit                        = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip                         = (fromIntegral $ fromMaybe 0 skip)
    transCache Nothing                = (Nothing, [])
    transCache (Just (hh, utxo, txs)) = (Just (hh, utxo), txs)

-- FIXME: is Word enough for length here?
searchHistory
    :: WalletWebMode m
    => CAddress -> Text -> Maybe Word -> Maybe Word -> m ([CTx], Word)
searchHistory cAddr search skip limit = first (filter $ txContainsTitle search) <$> getHistory cAddr skip limit

addHistoryTx
    :: (WebWalletModeDB m, MonadThrow m, MonadBlockchainInfo m)
    => CAddress
    -> CCurrency
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cAddr curr title desc wtx@(THEntry txId _ _ _ _) = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    addr <- decodeCAddressOrFail cAddr
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId txId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- fromMaybe meta <$> getTxMeta cAddr cId
    return $ mkCTx addr diff wtx meta'

newWallet :: WalletWebMode m => CPassPhrase -> CWalletInit -> m CWallet
newWallet cPassphrase CWalletInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAddr <- genSaveAddress passphrase cwBackupPhrase
    createWallet cAddr cwInitMeta
    getWallet cAddr

restoreWallet :: WalletWebMode m => CPassPhrase -> CWalletInit -> m CWallet
restoreWallet cPassphrase CWalletInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAddr <- genSaveAddress passphrase cwBackupPhrase
    getWalletMeta cAddr >>= maybe (createWallet cAddr cwInitMeta) (const walletExistsError)
    getWallet cAddr
  where
    walletExistsError = throwM $ Internal "Wallet with that mnemonics already exists"

updateWallet :: WalletWebMode m => CAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode m => CAddress -> CTxId -> CTxMeta -> m ()
updateTransaction = setWalletTransactionMeta

deleteWallet :: WalletWebMode m => CAddress -> m ()
deleteWallet cAddr = do
    deleteAddress =<< decodeCAddressOrFail cAddr
    removeWallet cAddr
  where
    deleteAddress addr = do
        idx <- getAddrIdx addr
        deleteSecretKey (fromIntegral idx) `catch` deleteErrHandler
    deleteErrHandler (PrimaryKey err) = throwM . Internal $
        sformat ("Error while deleting wallet: "%stext) err

-- NOTE: later we will have `isValidAddress :: CCurrency -> CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode m => Text -> CCurrency -> m Bool
isValidAddress sAddr ADA = pure $ isRight (decodeTextAddress sAddr)
isValidAddress _ _       = pure False

-- | Get last update info
nextUpdate :: WalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (Internal "No updates available")

applyUpdate :: WalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: WalletWebMode m => SendActions m -> CWalletRedeem -> m CTx
redeemAda sendActions CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal sendActions crWalletId seedBs
  where
    invalidBase64 = throwM . Internal $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend :: WalletWebMode m => SendActions m -> CPaperVendWalletRedeem -> m CTx
redeemAdaPaperVend sendActions CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal sendActions pvWalletId seedDecBs
  where
    invalidBase58 = throwM . Internal $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e = throwM . Internal $ "Invalid mnemonic: " <> toText e
    decryptionFailed e = throwM . Internal $ "Decryption failed: " <> show e

redeemAdaInternal :: WalletWebMode m => SendActions m -> CAddress -> ByteString -> m CTx
redeemAdaInternal sendActions walletId seedBs = do
    (_, redeemSK) <- maybeThrow (Internal "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    -- new redemption wallet
    walletB <- getWallet walletId

    -- send from seedAddress to walletB
    let dstCAddr = cwAddress walletB
    dstAddr <- decodeCAddressOrFail dstCAddr
    na <- getPeers
    etx <- submitRedemptionTx sendActions redeemSK (toList na) dstAddr
    case etx of
        Left err -> throwM . Internal $ "Cannot send redemption transaction: " <> err
        Right (TxAux {..}, redeemAddress, redeemBalance) -> do
            -- add redemption transaction to the history of new wallet
            addHistoryTx dstCAddr ADA "ADA redemption" ""
              (THEntry (hash taTx) taTx False [TxOut redeemAddress redeemBalance] Nothing)


reportingInitialized :: forall m. WalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs version (RInfo $ show cinit) `catch` handler
  where
    handler :: SomeException -> m ()
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall m. WalletWebMode m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- Ether.asks' (view rcReportServers)
    errors <- fmap lefts $ forM servers $ \serv ->
        try $ sendReport [fdFilePath $ cecUploadDump celcrash]
                         []
                         (RInfo $ show celcrash)
                         "daedalus"
                         version
                         (toString serv)
    whenNotNull errors $ handler . NE.head
  where
    fmt = ("Didn't manage to report electron crash "%shown%" because of exception "%shown)
    handler :: SomeException -> m ()
    handler e = logError $ sformat fmt celcrash e

rewrapError :: WalletWebMode m => m a -> m a
rewrapError = flip catches
    [ E.Handler $ \e@(Internal _)    -> throwM e
    , E.Handler $ \(SomeException e) -> throwM . Internal $ show e
    ]

importKey
    :: WalletWebMode m
    => Text
    -> m CWallet
importKey (toString -> fp) = do
    secret <- rewrapError $ readUserSecret fp
    let keys = secret ^. usKeys
    for_ keys $ \key -> do
        addSecretKey key
        let addr = makePubKeyAddress $ encToPublic key
            cAddr = addressToCAddress addr
        createWallet cAddr def

    case keys ^? ix 0 of
        Just key -> do
            let importedAddr = makePubKeyAddress $ encToPublic key
                importedCAddr = addressToCAddress importedAddr
            getWallet importedCAddr
        Nothing -> throwM . Internal $
            sformat ("No spending key found at "%build) fp

syncProgress :: WalletWebMode m => m SyncProgress
syncProgress = do
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

testResetAll :: WalletWebMode m => m ()
testResetAll | isDevelopment = deleteAllKeys >> testReset
             | otherwise     = throwM err403
  where
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0

---------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myAddresses :: MonadKeys m => m [Address]
myAddresses = map (makePubKeyAddress . encToPublic) <$> getSecretKeys

myCAddresses :: MonadKeys m => m [CAddress]
myCAddresses = map addressToCAddress <$> myAddresses

getAddrIdx :: WalletWebMode m => Address -> m Int
getAddrIdx addr = elemIndex addr <$> myAddresses >>= maybe notFound pure
  where notFound = throwM . Internal $
            sformat ("Address "%addressF%" is not found in wallet") $ addr

genSaveAddress
    :: WalletWebMode m
    => PassPhrase
    -> BackupPhrase
    -> m CAddress
genSaveAddress passphrase ph =
    addressToCAddress . makePubKeyAddress . encToPublic <$> genSaveSK
  where
    genSaveSK = do
        sk <- either keyFromPhraseFailed (pure . fst)
            $ safeKeysFromPhrase passphrase ph
        addSecretKey sk
        return sk
    keyFromPhraseFailed msg = throwM . Internal $ "Key creation from phrase failed: " <> msg

decodeCPassPhraseOrFail :: WalletWebMode m => CPassPhrase -> m PassPhrase
decodeCPassPhraseOrFail cpass =
    either (const . throwM $ Internal "Decoding of passphrase failed") return $
    cPassPhraseToPassPhrase cpass

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData CAddress where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CCurrency where
    parseUrlPiece = readEither . toString

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
