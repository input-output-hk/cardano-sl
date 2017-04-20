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
import           Control.Lens                  (ix, makeLenses, (.=))
import           Control.Monad.Catch           (SomeException, catches, try)
import qualified Control.Monad.Catch           as E
import           Control.Monad.State           (runStateT)
import           Data.Bits                     (setBit)
import           Data.Default                  (Default (def))
import           Data.List                     (elemIndex, (!!))
import qualified Data.List.NonEmpty            as NE
import           Data.Tagged                   (untag)
import qualified Data.Text                     as T
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Data.Time.Units               (Microsecond, Second)
import           Formatting                    (build, int, sformat, shown, stext, (%))
import qualified Formatting                    as F
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
import           System.Random                 (Random, randomIO)
import           System.Wlog                   (logDebug, logError, logInfo)

import           Data.ByteString.Base58        (bitcoinAlphabet, decodeBase58)
import           Pos.Aeson.ClientTypes         ()
import           Pos.Client.Txp.History        (TxHistoryAnswer (..), TxHistoryEntry (..))
import           Pos.Communication             (OutSpecs, SendActions, hoistSendActions,
                                                sendTxOuts, submitRedemptionTx, submitTx)
import           Pos.Constants                 (curSoftwareVersion, isDevelopment)
import           Pos.Core                      (Address (..), Coin, addressF,
                                                applyCoinPortion, decodeTextAddress,
                                                makePubKeyAddress, makeRedeemAddress,
                                                mkCoin, unsafeCoinPortionFromDouble,
                                                unsafeSubCoin)
import           Pos.Crypto                    (EncryptedSecretKey, PassPhrase,
                                                aesDecrypt, deriveAesKeyBS,
                                                deriveHDSecretKey, emptyPassphrase,
                                                encToPublic, fakeSigner, hash,
                                                noPassEncrypt, redeemDeterministicKeyGen,
                                                redeemToPublic, withSafeSigner,
                                                withSafeSigner)
import           Pos.DB.Limits                 (MonadDBLimits)
import           Pos.DHT.Model                 (getKnownPeers)
import           Pos.Genesis                   (genesisDevSecretKeys)
import           Pos.Reporting.MemState        (MonadReportingMem (..), rcReportServers)
import           Pos.Reporting.Methods         (sendReport, sendReportNodeNologs)
import           Pos.Ssc.Class                 (SscHelpersClass)
import           Pos.Txp.Core                  (TxOut (..), TxOutAux (..))
import           Pos.Util                      (maybeThrow)
import           Pos.Util.BackupPhrase         (BackupPhrase, mkBackupPhrase12,
                                                safeKeysFromPhrase, toSeed)
import           Pos.Util.UserSecret           (readUserSecret, usKeys)
import           Pos.Wallet.KeyStorage         (KeyError (..), MonadKeys (..),
                                                addSecretKey)
import           Pos.Wallet.WalletMode         (WalletMode, applyLastUpdate,
                                                blockchainSlotDuration, connectedPeers,
                                                getBalance, getTxHistory,
                                                localChainDifficulty,
                                                networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (Acc, CAccount (..), CAccountAddress (..),
                                                CAddress, CCurrency (ADA),
                                                CElectronCrashReport (..), CInitialized,
                                                CPassPhrase (..),
                                                CPostVendWalletRedeem (..), CProfile,
                                                CProfile (..), CTx (..), CTxId,
                                                CTxMeta (..), CUpdateInfo (..),
                                                CWallet (..), CWalletAddress (..),
                                                CWalletInit (..), CWalletMeta (..),
                                                CWalletRedeem (..), CWalletSet (..),
                                                CWalletSetInit (..), CWalletSetMeta (..),
                                                NotifyEvent (..), SyncProgress (..), WS,
                                                addressToCAddress, cAddressToAddress,
                                                cPassPhraseToPassPhrase, mkCCoin, mkCTx,
                                                mkCTxId, passPhraseToCPassPhrase,
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
                                                removeWSet, removeWallet, runWalletWebDB,
                                                setProfile, setWalletMeta,
                                                setWalletTransactionMeta, testReset,
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
    upgradeApplicationWS wsConn . serve walletApi <$> serv

walletServer
    :: forall ssc m.
       ( SscHelpersClass ssc
       , Monad m
       , MonadIO m
       , WalletWebMode ssc (WalletWebHandler m)
       )
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
    addInitialRichAccount sendActions' 0
    (`enter` servantHandlers @ssc sendActions') <$> nat
  where
    insertAddressMeta cAddr = do
        getWSetMeta cAddr >>= createWSet cAddr . fromMaybe def
    createUserProfile = pure $ CProfile mempty

----------------------------------------------------------------------------
-- Notifier
----------------------------------------------------------------------------

-- FIXME: this is really inefficient. Temporary solution
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
     apiPostVendRedeemAda
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
    apiGetWSet                  = (catchWalletError . getWSet)
    apiGetWSets                 = catchWalletError getWSets
    apiNewWSet                  = (\a -> catchWalletError . newWSet a)
    apiRestoreWSet              = (\a -> catchWalletError . newWSet a)
    apiImportKey                = catchWalletError . importKey
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
    apiGetHistory               = (\a b -> catchWalletError . getHistory @ssc a b)
    apiSearchHistory            = (\a b c d -> catchWalletError . searchHistory @ssc a b c d)
    apiNextUpdate               = catchWalletError nextUpdate
    apiApplyUpdate              = catchWalletError applyUpdate
    apiRedeemAda                = (\a -> catchWalletError . redeemADA sendActions a)
    apiPostVendRedeemAda        = (\a -> catchWalletError . postVendRedeemADA sendActions a)
    apiReportingInitialized     = catchWalletError . reportingInitialized
    apiReportingElectroncrash   = catchWalletError . reportingElectroncrash
    apiSettingsSlotDuration     = catchWalletError (fromIntegral <$> blockchainSlotDuration)
    apiSettingsSoftwareVersion  = catchWalletError (pure curSoftwareVersion)
    apiSettingsSyncProgress     = catchWalletError syncProgress

    catchWalletError            = catchOtherError . try
    catchOtherError             = E.handleAll $ \e -> do
        logError $ sformat ("Uncaught error in wallet method: "%shown) e
        throwM e

-- getAddresses :: WalletWebMode ssc m => m [CAddress]
-- getAddresses = map addressToCAddress <$> myAddresses

-- getBalances :: WalletWebMode ssc m => m [(CAddress, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getUserProfile :: WalletWebMode ssc m => m CProfile
getUserProfile = getProfile >>= maybeThrow (Internal "No user profile")

updateUserProfile :: WalletWebMode ssc m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getAccountBalance :: WalletWebMode ssc m => CAccountAddress -> m Coin
getAccountBalance cAccAddr =
    getBalance <=< decodeCAddressOrFail $ caaAddress cAccAddr

getAccount :: WalletWebMode ssc m => CAccountAddress -> m CAccount
getAccount cAddr = do
    balance <- mkCCoin <$> getAccountBalance cAddr
    return $ CAccount cAddr balance

getWalletAccAddrsOrThrow
    :: WalletWebMode ssc m
    => CWalletAddress -> m [CAccountAddress]
getWalletAccAddrsOrThrow wCAddr =
    getWalletAccounts wCAddr >>= maybeThrow noWallet
  where
    noWallet =
        Internal $ sformat ("No wallet with address "%build%" found") wCAddr

getAccounts :: WalletWebMode ssc m => CWalletAddress -> m [CAccount]
getAccounts = getWalletAccAddrsOrThrow >=> mapM getAccount

getWallet :: WalletWebMode ssc m => CWalletAddress -> m CWallet
getWallet cAddr = do
    accounts <- getAccounts cAddr
    meta     <- getWalletMeta cAddr >>= maybeThrow noWallet
    pure $ CWallet cAddr meta accounts
  where
    noWallet =
        Internal $ sformat ("No wallet with address "%build%" found") cAddr

getWSet :: WalletWebMode ssc m => CAddress WS -> m CWalletSet
getWSet cAddr = do
    meta       <- getWSetMeta cAddr >>= maybeThrow noWSet
    walletsNum <- length <$> getWallets (Just cAddr)
    pure $ CWalletSet cAddr meta walletsNum
  where
    noWSet = Internal $
        sformat ("No wallet set with address "%build%" found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: WalletWebMode ssc m => CAddress w -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode ssc m => Maybe (CAddress WS) -> m [CWallet]
getWallets mCAddr = do
    whenJust mCAddr $ \cAddr -> getWSetMeta cAddr `whenNothingM_` noWSet cAddr
    mapM getWallet . filterByAddr mCAddr =<< getWalletAddresses
  where
    filterByAddr = maybe identity $ \cAddr ->
        filter $ (== cAddr) . cwaWSAddress
    noWSet cAddr = throwM . Internal $
        sformat ("No wallet set with address "%build%" found") cAddr

getWSets :: WalletWebMode ssc m => m [CWalletSet]
getWSets = getWSetAddresses >>= mapM getWSet

decodeCPassPhraseOrFail :: WalletWebMode ssc m => CPassPhrase -> m PassPhrase
decodeCPassPhraseOrFail cpass =
    either (const . throwM $ Internal "Decoding of passphrase failed") return $
    cPassPhraseToPassPhrase cpass

send
    :: (WalletWebMode ssc m)
    => SendActions m
    -> CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> m [CTx]
send sendActions cpass srcCAddr dstCAddr c =
    sendExtended sendActions cpass srcCAddr dstCAddr c ADA mempty mempty

sendExtended
    :: (WalletWebMode ssc m)
    => SendActions m
    -> CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> CCurrency
    -> Text
    -> Text
    -> m [CTx]
sendExtended sendActions cpassphrase srcWallet dstAccount c curr title desc = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    dstAddr <- decodeCAddressOrFail dstAccount
    allAccounts <- getWalletAccAddrsOrThrow srcWallet
    distr@(remaining, spendings) <- selectSrcAccounts c allAccounts
    logDebug $ buildDistribution distr
    mRemTx <-
        if remaining == mkCoin 0
            then return Nothing
            else do
                remCAddr <- caAddress <$> newAccount cpassphrase srcWallet
                remAddr <- decodeCAddressOrFail $ caaAddress remCAddr
                let remTx = TxOutAux (TxOut remAddr remaining) []
                return $ Just (remTx, remCAddr)
    let mainTxs =
            spendings <&> \(srcAcc, coin) ->
                (srcAcc, TxOutAux (TxOut dstAddr coin) [])
        txsWithRem =
            -- attach remaining money destination account to last selected
            -- source account
            zipWith
                (\(srcAcc, txs) remTxs -> (srcAcc, txs :| remTxs))
                (reverse mainTxs)
                (maybe mempty (one . fst) mRemTx : repeat [])
    na <- getKnownPeers
    forM txsWithRem $ uncurry (sendDo na passphrase)
  where
    selectSrcAccounts
        :: WalletWebMode ssc m
        => Coin -> [CAccountAddress] -> m (Coin, [(CAccountAddress, Coin)])
    selectSrcAccounts reqCoins accounts =
        case accounts of
            _
                | reqCoins == mkCoin 0 ->
                    throwM $ Internal "Spending non-positive amount of money!"
            [] ->
                throwM . Internal $
                sformat ("Not enough money (need " %build % " more)") reqCoins
            acc:accs -> do
                balance <- getAccountBalance acc
                case () of
                    _
                        | balance == mkCoin 0 || caaAddress acc == dstAccount ->
                            selectSrcAccounts reqCoins accs
                        | balance < reqCoins -> do
                            let remCoins = reqCoins `unsafeSubCoin` balance
                            ((acc, balance) :) <<$>>
                                selectSrcAccounts remCoins accs
                        | otherwise ->
                            return
                                ( balance `unsafeSubCoin` reqCoins
                                , [(acc, reqCoins)])
    sendDo na passphrase srcAccount txs = do
        sk <- getSKByAccAddr passphrase srcAccount
        srcAccAddr <- decodeCAddressOrFail (caaAddress srcAccount)
        let dstAddrs = toList txs <&> \(TxOutAux (TxOut addr _) _) -> addr
        withSafeSigner sk (return passphrase) $ \mss -> do
            ss  <- mss `whenNothing` throwM (Internal "Passphrase doesn't match")
            etx <- submitTx sendActions ss na txs
            case etx of
                Left err ->
                    throwM . Internal $
                    sformat ("Cannot send transaction: " %stext) err
                Right (tx, _, _) -> do
                    logInfo $
                        sformat
                            ("Successfully spent money from " %addressF %
                             " address on " %listF ", " addressF)
                            srcAccAddr
                            dstAddrs
                    -- TODO: this should be removed in production
                    let txHash = hash tx
                        addHistory addr isInput =
                            addHistoryTx
                                addr
                                curr
                                title
                                desc
                                (THEntry txHash tx isInput Nothing srcAccAddr)
                    removeAccount srcAccount
                    forM_ dstAddrs $ \dstAddr ->
                        let dstCAddr = addressToCAddress dstAddr
                        in addHistory dstCAddr False
                    addHistory (caaAddress srcAccount) True
    listF separator formatter =
        F.later $ mconcat . intersperse separator . map (F.bprint formatter)
    buildDistribution (remaining, spendings) =
        let entries =
                spendings <&> \(CAccountAddress {..}, coin) ->
                    F.bprint (build % ": " %build) coin caaAddress
            remains = F.bprint ("Remaining: " %build) remaining
        in sformat
               ("Transaction input distribution:\n" %listF "\n" build %
                "\n" %build)
               entries
               remains

getHistory
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => CWalletAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory wAddr skip limit = do
    cAccAddrs <- getWalletAccAddrsOrThrow wAddr
    accAddrs <- forM cAccAddrs (decodeCAddressOrFail . caaAddress)
    cHistory <-
        do (minit, cachedTxs) <- transCache <$> getHistoryCache wAddr

           TxHistoryAnswer {..} <- untag @ssc getTxHistory accAddrs minit

           -- Add allowed portion of result to cache
           let fullHistory = taHistory <> cachedTxs
               lenHistory = length taHistory
               cached = drop (lenHistory - taCachedNum) taHistory
           unless (null cached) $
               updateHistoryCache
                   wAddr
                   taLastCachedHash
                   taCachedUtxo
                   (cached <> cachedTxs)

           forM fullHistory $ \thEntry@THEntry {..} ->
               let srcCAddr = addressToCAddress _thInputAddr
               in addHistoryTx srcCAddr ADA mempty mempty thEntry
    pure (paginate cHistory, fromIntegral $ length cHistory)
  where
    paginate = take defaultLimit . drop defaultSkip
    defaultLimit = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip = (fromIntegral $ fromMaybe 0 skip)
    transCache Nothing                = (Nothing, [])
    transCache (Just (hh, utxo, txs)) = (Just (hh, utxo), txs)

-- FIXME: is Word enough for length here?
searchHistory
    :: forall ssc m.
       (SscHelpersClass ssc, WalletWebMode ssc m)
    => CWalletAddress
    -> Text
    -> Maybe (CAddress Acc)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
searchHistory wAddr search mAccAddr skip limit =
    first (filter fits) <$> getHistory @ssc wAddr skip limit
  where
    fits ctx = txContainsTitle search ctx
            && maybe True (== ctAccAddress ctx) mAccAddr

addHistoryTx
    :: WalletWebMode ssc m
    => CAddress Acc
    -> CCurrency
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cAddr curr title desc wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    addr <- decodeCAddressOrFail cAddr
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- maybe meta identity <$> getTxMeta cAddr cId
    return $ mkCTx addr diff wtx meta'

newAccount :: WalletWebMode ssc m => CPassPhrase -> CWalletAddress -> m CAccount
newAccount cPassphrase cWAddr = do
    -- check wallet exists
    _ <- getWallet cWAddr

    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAccAddr <- genUniqueAccountAddress passphrase cWAddr
    addAccount cAccAddr
    getAccount cAccAddr

newWallet :: WalletWebMode ssc m => CPassPhrase -> CWalletInit -> m CWallet
newWallet cPassphrase CWalletInit {..} = do
    -- check wallet set exists
    _ <- getWSet cwInitWSetId

    cAddr <- genUniqueWalletAddress cwInitWSetId
    createWallet cAddr cwInitMeta
    () <$ newAccount cPassphrase cAddr
    getWallet cAddr

createWSetSafe
    :: WalletWebMode ssc m
    => CAddress WS -> CWalletSetMeta -> m CWalletSet
createWSetSafe cAddr wsMeta = do
    wSetExists <- isJust <$> getWSetMeta cAddr
    when wSetExists $
        throwM $ Internal "Wallet set with that mnemonics already exists"
    createWSet cAddr wsMeta
    getWSet cAddr

newWSet :: WalletWebMode ssc m => CPassPhrase -> CWalletSetInit -> m CWalletSet
newWSet cPassphrase CWalletSetInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    let CWalletSetMeta {..} = cwsInitMeta
    cAddr <- genSaveRootAddress passphrase cwsBackupPhrase
    createWSetSafe cAddr cwsInitMeta

updateWallet :: WalletWebMode ssc m => CWalletAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode ssc m => CAddress Acc -> CTxId -> CTxMeta -> m ()
updateTransaction = setWalletTransactionMeta

deleteWSet :: WalletWebMode ssc m => CAddress WS -> m ()
deleteWSet wsAddr = do
    wallets <- getWallets (Just wsAddr)
    mapM_ (deleteWallet . cwAddress) wallets
    deleteAddress wsAddr
    removeWSet wsAddr
  where
    deleteAddress addr = do
        idx <- getAddrIdx addr
        deleteSecretKey (fromIntegral idx) `catch` deleteErrHandler
    deleteErrHandler (PrimaryKey err) = throwM . Internal $
        sformat ("Error while deleting wallet set: "%stext) err

deleteWallet :: WalletWebMode ssc m => CWalletAddress -> m ()
deleteWallet wAddr = do
    mapM_ deleteAccount =<< getWalletAccAddrsOrThrow wAddr
    removeWallet wAddr

deleteAccount :: WalletWebMode ssc m => CAccountAddress -> m ()
deleteAccount = removeAccount

-- NOTE: later we will have `isValidAddress :: CCurrency -> CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode ssc m => Text -> CCurrency -> m Bool
isValidAddress sAddr ADA =
    pure . either (const False) (const True) $ decodeTextAddress sAddr
isValidAddress _ _       = pure False

-- | Get last update info
nextUpdate :: WalletWebMode ssc m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (Internal "No updates available")

applyUpdate :: WalletWebMode ssc m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemADA :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CWalletRedeem -> m CTx
redeemADA sendActions cpassphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemADAInternal sendActions cpassphrase crWalletId seedBs
  where
    invalidBase64 = throwM . Internal $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
postVendRedeemADA :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CPostVendWalletRedeem -> m CTx
postVendRedeemADA sendActions cpassphrase CPostVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemADAInternal sendActions cpassphrase pvWalletId seedDecBs
  where
    invalidBase58 = throwM . Internal $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e = throwM . Internal $ "Invalid mnemonic: " <> toText e
    decryptionFailed e = throwM . Internal $ "Decryption failed: " <> show e

redeemADAInternal :: WalletWebMode ssc m => SendActions m -> CPassPhrase -> CWalletAddress -> ByteString -> m CTx
redeemADAInternal sendActions cpassphrase walletId seedBs = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    (_, redeemSK) <- maybeThrow (Internal "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    -- new redemption wallet
    _ <- getWallet walletId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstCAddr <- genUniqueAccountAddress passphrase walletId
    dstAddr <- decodeCAddressOrFail $ caaAddress dstCAddr
    na <- getKnownPeers
    etx <- submitRedemptionTx sendActions redeemSK na dstAddr
    case etx of
        Left err -> throwM . Internal $ "Cannot send redemption transaction: " <> err
        Right (tx, _, _) -> do
            -- add redemption transaction to the history of new wallet
            addHistoryTx (caaAddress dstCAddr) ADA "ADA redemption" ""
              (THEntry (hash tx) tx False Nothing srcAddr)

reportingInitialized
    :: forall ssc m.
       WalletWebMode ssc m
    => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs version (RInfo $ show cinit) `catch` handler
  where
    handler :: SomeException -> m ()
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall ssc m. WalletWebMode ssc m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- view rcReportServers <$> askReportingContext
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

rewrapError :: WalletWebMode ssc m => m a -> m a
rewrapError = flip catches
    [ E.Handler $ \e@(Internal _)    -> throwM e
    , E.Handler $ \(SomeException e) -> throwM . Internal $ show e
    ]

importKey
    :: WalletWebMode ssc m
    => Text
    -> m CWalletSet
importKey (toString -> fp) = do
    secret <- rewrapError $ readUserSecret fp
    let keys = secret ^. usKeys
    importedWSets <-
        forM keys $ \key -> do
            let addr = makePubKeyAddress $ encToPublic key
                wsAddr = addressToCAddress addr
            createWSetSafe wsAddr def <* addSecretKey key
    maybeThrow noKey $ head importedWSets
  where
    noKey = Internal $ sformat ("No spending key found at " %build) fp

-- This method is a temporal hack.
-- We spend money from /accounts/, which have randomly generated addresses.
-- To create account with some initial amount of money, we create wallet set
-- with @key = some genesis key@, and send half of savings to newly created
-- account.
addInitialRichAccount :: WalletWebMode ssc m => SendActions m -> Int -> m ()
addInitialRichAccount sendActions keyId =
    when isDevelopment . E.handleAll handler $ do
        key <- maybeThrow noKey (genesisDevSecretKeys ^? ix keyId)
        let enKey = noPassEncrypt key

        let addr = makePubKeyAddress $ encToPublic enKey
            wsAddr = addressToCAddress addr

        balance <- getBalance addr
        let coinsToSend = applyCoinPortion (unsafeCoinPortionFromDouble 0.5) balance

        let cpass  = passPhraseToCPassPhrase emptyPassphrase
            backup = mkBackupPhrase12 $ sformat build <$> "nyan-forever"
            wsMeta = CWalletSetMeta "Precreated wallet set full of money" backup
            wMeta  = def{ cwName = "Initial wallet" }

        deleteWSet wsAddr `catchAll` \_ -> return ()
        _ <- createWSetSafe wsAddr wsMeta
        addSecretKey enKey
        wAddr    <- cwAddress <$> newWallet cpass (CWalletInit wMeta wsAddr)
        accounts <- getAccounts wAddr
        accAddr  <- maybeThrow noAccount . head $ caAddress <$> accounts

        -- send all money from wallet set (corresponds to genesis address)
        -- to its account
        na          <- getKnownPeers
        let signer   = fakeSigner key
        let dstCAddr = caaAddress accAddr
        dstAddr     <- decodeCAddressOrFail dstCAddr
        let tx       = TxOutAux (TxOut dstAddr coinsToSend) []
        etx         <- submitTx sendActions signer na (one tx)
        case etx of
            Left err ->
                throwM . Internal $ sformat ("Cannot send transaction \
                                    \for genesis account: "%stext) err
            Right _  ->
                logDebug $ sformat ("Spent "%build%" from genesis address #"
                           %int) coinsToSend keyId
  where
    noKey = Internal $ sformat ("No genesis key #"%build) keyId
    noAccount = Internal "No account created with wallet creation!"
    handler = logError . sformat ("Creation of init account failed: "%build)

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

myRootAddresses :: MonadKeys m => m [CAddress WS]
myRootAddresses =
    addressToCAddress . makePubKeyAddress . encToPublic <<$>> getSecretKeys

getAddrIdx :: WalletWebMode ssc m => CAddress WS -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          Internal $ sformat ("Address "%build%" is not found in wallet") addr

getSKByAddr
    :: WalletWebMode ssc m
    => CAddress WS
    -> m EncryptedSecretKey
getSKByAddr cAddr = do
    idx <- getAddrIdx cAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    return sk

getSKByAccAddr
    :: WalletWebMode ssc m
    => PassPhrase
    -> CAccountAddress
    -> m EncryptedSecretKey
getSKByAccAddr passphrase accAddr@CAccountAddress {..} = do
    (addr, accKey) <-
        deriveAccountSK passphrase (walletAddrByAccount accAddr) caaAccountIndex
    let accCAddr = addressToCAddress addr
    if accCAddr /= caaAddress
        then throwM . Internal $ "Account is contradictory!"
        else return accKey

genSaveRootAddress
    :: WalletWebMode ssc m
    => PassPhrase
    -> BackupPhrase
    -> m (CAddress WS)
genSaveRootAddress passphrase ph =
    addressToCAddress . makePubKeyAddress . encToPublic <$> genSaveSK
  where
    genSaveSK = do
        sk <- either keyFromPhraseFailed (pure . fst)
            $ safeKeysFromPhrase passphrase ph
        addSecretKey sk
        return sk
    keyFromPhraseFailed msg = throwM . Internal $ "Key creation from phrase failed: " <> msg

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
    => CAddress WS
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
                   (doesAccountExist . caaAddress)
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
    wsKey       <- getSKByAddr cwaWSAddress
    let wKey     = deriveHDSecretKey passphrase wsKey cwaIndex
    let accKey  = deriveHDSecretKey passphrase wKey accIndex
    let accAddr = makePubKeyAddress $ encToPublic accKey
    return (accAddr, accKey)

    -- TODO [CSM-175]: This is true way to generate keypair, since
    -- public key should keep keypath in its attribute.
    -- But for some reason it makes tx sending from generated account fail
    --
    -- Note: perhaps it's not the only place where `createHDAddressH` should be
    -- actually used

    -- let hdPass   = deriveHDPassphrase $ encToPublic wsKey
    -- return $ createHDAddressH passphrase hdPass wKey [cwaIndex] accIndex


----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData (CAddress w) where
    parseUrlPiece = fmap addressToCAddress . decodeTextAddress

instance FromHttpApiData CWalletAddress where
    parseUrlPiece url =
        case T.splitOn "@" url of
            [part1, part2] -> do
                cwaWSAddress <- parseUrlPiece part1
                cwaIndex     <- maybe (Left "Invalid wallet index") Right $
                                readMaybe $ toString part2
                return CWalletAddress{..}
            _ -> Left "Expected 2 parts separated by '#'"

instance FromHttpApiData CAccountAddress where
    parseUrlPiece url =
        case T.splitOn "@" url of
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
