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
import           Control.Monad.Catch           (try)
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.State           (runStateT)
import qualified Data.ByteString.Base64        as B64
import           Data.Default                  (Default (def))
import           Data.List                     (elemIndex, (!!))
import           Data.Tagged                   (untag)
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
import           System.Wlog                   (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes         ()
import           Pos.Communication.Protocol    (OutSpecs, SendActions, hoistSendActions)
import           Pos.Constants                 (curSoftwareVersion, isDevelopment)
import           Pos.Core                      (Address, Coin, addressF, coinF,
                                                decodeTextAddress, makePubKeyAddress,
                                                mkCoin)
import           Pos.Crypto                    (emptyPassphrase, encToPublic, fakeSigner,
                                                hash, redeemDeterministicKeyGen,
                                                toEncrypted, toPublic, withSafeSigner,
                                                withSafeSigner)
import           Pos.DB.Limits                 (MonadDBLimits)
import           Pos.DHT.Model                 (getKnownPeers)
import           Pos.Reporting.MemState        (MonadReportingMem (..))
import           Pos.Reporting.Methods         (sendReportNodeNologs)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Txp.Core                  (TxOut (..), TxOutAux (..))
import           Pos.Util                      (maybeThrow)
import           Pos.Util.BackupPhrase         (BackupPhrase, safeKeysFromPhrase)
import           Pos.Util.UserSecret           (readUserSecret, usKeys, usPrimKey)
import           Pos.Wallet.KeyStorage         (KeyError (..), MonadKeys (..),
                                                addSecretKey)
import           Pos.Wallet.Tx                 (sendTxOuts, submitRedemptionTx, submitTx)
import           Pos.Wallet.Tx.Pure            (TxHistoryEntry (..))
import           Pos.Wallet.WalletMode         (TxHistoryAnswer (..), WalletMode,
                                                applyLastUpdate, blockchainSlotDuration,
                                                connectedPeers, getBalance, getTxHistory,
                                                localChainDifficulty,
                                                networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (CAddress, CCurrency (ADA), CInitialized,
                                                CProfile, CProfile (..), CTx, CTxId,
                                                CTxMeta (..), CUpdateInfo (..),
                                                CWallet (..), CWalletInit (..),
                                                CWalletMeta (..), CWalletRedeem (..),
                                                NotifyEvent (..), SyncProgress (..),
                                                addressToCAddress, cAddressToAddress,
                                                mkCTx, mkCTxId, toCUpdateInfo,
                                                txContainsTitle, txIdToCTxId)
import           Pos.Wallet.Web.Error          (WalletError (..))
import           Pos.Wallet.Web.Server.Sockets (MonadWalletWebSockets (..),
                                                WalletWebSockets, closeWSConnection,
                                                getWalletWebSockets, initWSConnection,
                                                notify, runWalletWS, upgradeApplicationWS)
import           Pos.Wallet.Web.State          (testReset)
import           Pos.Wallet.Web.State          (MonadWalletWebDB (..), WalletWebDB,
                                                addOnlyNewTxMeta, addUpdate, closeState,
                                                createWallet, getHistoryCache,
                                                getNextUpdate, getProfile, getTxMeta,
                                                getWalletMeta, getWalletState, openState,
                                                removeNextUpdate, removeWallet,
                                                runWalletWebDB, setProfile, setWalletMeta,
                                                setWalletTransactionMeta,
                                                updateHistoryCache)
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
    myCAddresses >>= mapM_ insertAddressMeta
    (`enter` servantHandlers @ssc sendActions') <$> nat
  where
    insertAddressMeta cAddr =
        getWalletMeta cAddr >>= createWallet cAddr . fromMaybe def
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
    --     cAddresses <- myCAddresses
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
     apiImportKey
    :<|>
     apiRestoreWallet
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
    apiGetWallet                = (catchWalletError . getWallet)
    apiGetWallets               = catchWalletError getWallets
    apiUpdateWallet             = (\a -> catchWalletError . updateWallet a)
    apiNewWallet                = catchWalletError . newWallet
    apiDeleteWallet             = catchWalletError . deleteWallet
    apiImportKey                = catchWalletError . importKey sendActions
    apiRestoreWallet            = catchWalletError . restoreWallet
    apiIsValidAddress           = (\a -> catchWalletError . isValidAddress a)
    apiGetUserProfile           = catchWalletError getUserProfile
    apiUpdateUserProfile        = catchWalletError . updateUserProfile
    apiTxsPayments              = (\a b -> catchWalletError . send sendActions a b)
    apiTxsPaymentsExt           = (\a b c d e -> catchWalletError . sendExtended sendActions a b c d e)
    apiUpdateTransaction        = (\a b -> catchWalletError . updateTransaction a b)
    apiGetHistory               = (\a b -> catchWalletError . getHistory @ssc a b )
    apiSearchHistory            = (\a b c -> catchWalletError . searchHistory @ssc a b c)
    apiNextUpdate               = catchWalletError nextUpdate
    apiApplyUpdate              = catchWalletError applyUpdate
    apiRedeemAda                = catchWalletError . redeemADA sendActions
    apiReportingInitialized     = catchWalletError . reportingInitialized
    apiSettingsSlotDuration     = catchWalletError (fromIntegral <$> blockchainSlotDuration)
    apiSettingsSoftwareVersion  = catchWalletError (pure curSoftwareVersion)
    apiSettingsSyncProgress     = catchWalletError syncProgress

    catchWalletError            = try
-- getAddresses :: WalletWebMode ssc m => m [CAddress]
-- getAddresses = map addressToCAddress <$> myAddresses

-- getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getUserProfile :: WalletWebMode ssc m => m CProfile
getUserProfile = getProfile >>= maybe noProfile pure
  where
    noProfile = throwM $ Internal "No user profile"

updateUserProfile :: WalletWebMode ssc m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getWallet :: WalletWebMode ssc m => CAddress -> m CWallet
getWallet cAddr = do
    balance <- getBalance =<< decodeCAddressOrFail cAddr
    meta <- getWalletMeta cAddr >>= maybe noWallet pure
    pure $ CWallet cAddr balance meta
  where
    noWallet = throwM . Internal $
        sformat ("No wallet with address "%build%" is found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: WalletWebMode ssc m => CAddress -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode ssc m => m [CWallet]
getWallets = join $ mapM getWallet <$> myCAddresses

send :: WalletWebMode ssc m => SendActions m -> CAddress -> CAddress -> Coin -> m CTx
send sendActions srcCAddr dstCAddr c =
    sendExtended sendActions srcCAddr dstCAddr c ADA mempty mempty

sendExtended :: WalletWebMode ssc m => SendActions m -> CAddress -> CAddress -> Coin -> CCurrency -> Text -> Text -> m CTx
sendExtended sendActions srcCAddr dstCAddr c curr title desc = do
    srcAddr <- decodeCAddressOrFail srcCAddr
    dstAddr <- decodeCAddressOrFail dstCAddr
    idx <- getAddrIdx srcAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    na <- getKnownPeers
    withSafeSigner sk (return emptyPassphrase) $ \ss -> do
        etx <- submitTx sendActions ss na (one $ TxOutAux (TxOut dstAddr c) [])
        case etx of
            Left err -> throwM . Internal $ sformat ("Cannot send transaction: "%stext) err
            Right (tx, _, _) -> do
                logInfo $
                    sformat ("Successfully sent "%coinF%" from "%ords%" address to "%addressF)
                    c idx dstAddr
                -- TODO: this should be removed in production
                let txHash = hash tx
                () <$ addHistoryTx dstCAddr curr title desc (THEntry txHash tx False Nothing)
                addHistoryTx srcCAddr curr title desc (THEntry txHash tx True Nothing)

getHistory
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => CAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory cAddr skip limit = do
    (minit, cachedTxs) <- transCache <$> getHistoryCache cAddr

    TxHistoryAnswer {..} <- flip (untag @ssc getTxHistory) minit
        =<< decodeCAddressOrFail cAddr

    -- Add allowed portion of result to cache
    let fullHistory = taHistory <> cachedTxs
        lenHistory = length taHistory
        cached = drop (lenHistory - taCachedNum) taHistory

    unless (null cached) $
        updateHistoryCache cAddr taLastCachedHash taCachedUtxo (cached <> cachedTxs)

    cHistory <- mapM (addHistoryTx cAddr ADA mempty mempty) fullHistory
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
    => CAddress -> Text -> Maybe Word -> Maybe Word -> m ([CTx], Word)
searchHistory cAddr search skip limit = first (filter $ txContainsTitle search) <$> getHistory @ssc cAddr skip limit

addHistoryTx
    :: WalletWebMode ssc m
    => CAddress
    -> CCurrency
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cAddr curr title desc wtx@(THEntry txId _ _ _) = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    addr <- decodeCAddressOrFail cAddr
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId txId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- maybe meta identity <$> getTxMeta cAddr cId
    return $ mkCTx addr diff wtx meta'

newWallet :: WalletWebMode ssc m => CWalletInit -> m CWallet
newWallet CWalletInit {..} = do
    cAddr <- genSaveAddress cwBackupPhrase
    createWallet cAddr cwInitMeta
    getWallet cAddr

restoreWallet :: WalletWebMode ssc m => CWalletInit -> m CWallet
restoreWallet CWalletInit {..} = do
    cAddr <- genSaveAddress cwBackupPhrase
    getWalletMeta cAddr >>= maybe (createWallet cAddr cwInitMeta) (const walletExistsError)
    getWallet cAddr
  where
    walletExistsError = throwM $ Internal "Wallet with that mnemonics already exists"

updateWallet :: WalletWebMode ssc m => CAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode ssc m => CAddress -> CTxId -> CTxMeta -> m ()
updateTransaction = setWalletTransactionMeta

deleteWallet :: WalletWebMode ssc m => CAddress -> m ()
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
isValidAddress :: WalletWebMode ssc m => Text -> CCurrency -> m Bool
isValidAddress sAddr ADA = pure . either (const False) (const True) $ decodeTextAddress sAddr
isValidAddress _ _       = pure False

-- | Get last update info
nextUpdate :: WalletWebMode ssc m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (Internal "No updates available")

applyUpdate :: WalletWebMode ssc m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemADA :: WalletWebMode ssc m => SendActions m -> CWalletRedeem -> m CTx
redeemADA sendActions CWalletRedeem {..} = do
    seedBs <- either
        (\e -> throwM $ Internal ("Seed is invalid base64 string: " <> toText e))
        pure $ B64.decode (encodeUtf8 crSeed)
    (_, redeemSK) <- maybeThrow (Internal "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    -- new redemption wallet
    walletB <- getWallet crWalletId

    -- send from seedAddress to walletB
    let dstCAddr = cwAddress walletB
    dstAddr <- decodeCAddressOrFail dstCAddr
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

importKey :: WalletWebMode ssc m => SendActions m -> Text -> m CWallet
importKey sendActions (toString -> fp) = do
    secret <- readUserSecret fp
    let keys = case secret ^. usPrimKey of
            Nothing -> secret ^. usKeys
            Just k  -> toEncrypted k : secret ^. usKeys
    forM_ keys $ \key -> do
        addSecretKey key
        let addr = makePubKeyAddress $ encToPublic key
            cAddr = addressToCAddress addr
        createWallet cAddr def

    let importedAddr = makePubKeyAddress $ encToPublic (keys !! 0)
        importedCAddr = addressToCAddress importedAddr
    when isDevelopment $ do
        psk <- maybeThrow (Internal "No primary key is present!")
               =<< getPrimaryKey
        let pAddr = makePubKeyAddress $ toPublic psk
        primaryBalance <- getBalance pAddr
        when (primaryBalance > mkCoin 0) $ do
            na <- getKnownPeers
            etx <- submitTx sendActions (fakeSigner psk) na
                       (one $ TxOutAux (TxOut importedAddr primaryBalance) [])
            case etx of
                Left err -> throwM . Internal $ "Cannot transfer funds from genesis key" <> err
                Right (tx, _, _) -> void $
                    addHistoryTx importedCAddr ADA "Transfer money from genesis key" ""
                        (THEntry (hash tx) tx False Nothing)
    getWallet importedCAddr

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
        keyNum <- fromIntegral . pred . length <$> getSecretKeys
        sequence_ $ replicate keyNum $ deleteSecretKey 1

---------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

myAddresses :: MonadKeys m => m [Address]
myAddresses = map (makePubKeyAddress . encToPublic) <$> getSecretKeys

myCAddresses :: MonadKeys m => m [CAddress]
myCAddresses = map addressToCAddress <$> myAddresses

getAddrIdx :: WalletWebMode ssc m => Address -> m Int
getAddrIdx addr = elemIndex addr <$> myAddresses >>= maybe notFound pure
  where notFound = throwM . Internal $
            sformat ("Address "%addressF%" is not found in wallet") $ addr

genSaveAddress :: WalletWebMode ssc m => BackupPhrase -> m CAddress
genSaveAddress ph = addressToCAddress . makePubKeyAddress . encToPublic <$> genSaveSK ph
  where
    genSaveSK ph' = do
        let sk = fst $ safeKeysFromPhrase emptyPassphrase ph'
        addSecretKey sk
        return sk

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
-- we are not checking is receaved Text really vald CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CCurrency where
    parseUrlPiece = first fromString . readEither . toString
