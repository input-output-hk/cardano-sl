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
import           Control.Monad.Catch           (SomeException, try)
import qualified Control.Monad.Catch           as E
import           Control.Monad.State           (runStateT)
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
import           Pos.Communication             (NodeId, OutSpecs, SendActions,
                                                hoistSendActions, sendTxOuts, submitMTx,
                                                submitRedemptionTx, submitTx)
import           Pos.Constants                 (curSoftwareVersion, isDevelopment)
import           Pos.Core                      (Address (..), Coin, addressF,
                                                applyCoinPortion, decodeTextAddress,
                                                makePubKeyAddress, makeRedeemAddress,
                                                mkCoin, unsafeCoinPortionFromDouble,
                                                unsafeSubCoin)
import           Pos.Crypto                    (EncryptedSecretKey, PassPhrase,
                                                aesDecrypt, changeEncPassphrase,
                                                checkPassMatches, deriveAesKeyBS,
                                                emptyPassphrase, encToPublic, fakeSigner,
                                                hash, isNonHardened, noPassEncrypt,
                                                redeemDeterministicKeyGen, redeemToPublic,
                                                withSafeSigner, withSafeSigner)
import           Pos.DB.Limits                 (MonadDBLimits)
import           Pos.Genesis                   (genesisDevSecretKeys)
import           Pos.Reporting.MemState        (MonadReportingMem, askReportingContext,
                                                rcReportServers)
import           Pos.Reporting.Methods         (sendReport, sendReportNodeNologs)
import           Pos.Txp.Core                  (TxOut (..), TxOutAux (..))
import           Pos.Util                      (maybeThrow)
import           Pos.Util.BackupPhrase         (BackupPhrase, safeKeysFromPhrase, toSeed)
import           Pos.Wallet.KeyStorage         (MonadKeys (..), addSecretKey)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.WalletMode         (WalletMode, applyLastUpdate,
                                                blockchainSlotDuration, connectedPeers,
                                                getBalance, getTxHistory,
                                                localChainDifficulty,
                                                networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Api            (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes    (Acc, CAccount (..), CAccountAddress (..),
                                                CAddress, CCurrency (ADA),
                                                CElectronCrashReport (..), CInitialized,
                                                CPaperVendWalletRedeem (..),
                                                CPassPhrase (..), CProfile, CProfile (..),
                                                CTx (..), CTxId, CTxMeta (..),
                                                CUpdateInfo (..), CWallet (..),
                                                CWalletAddress (..), CWalletInit (..),
                                                CWalletMeta (..), CWalletRedeem (..),
                                                CWalletSet (..), CWalletSetInit (..),
                                                CWalletSetMeta (..), MCPassPhrase,
                                                NotifyEvent (..), SyncProgress (..), WS,
                                                WalletUserSecret (..), addressToCAddress,
                                                cAddressToAddress,
                                                cPassPhraseToPassPhrase, mkCCoin, mkCTx,
                                                mkCTxId, readWalletUserSecret,
                                                toCUpdateInfo, txContainsTitle,
                                                txIdToCTxId, walletAddrByAccount)
import           Pos.Wallet.Web.Error          (WalletError (..))
import           Pos.Wallet.Web.Server.Sockets (MonadWalletWebSockets, WalletWebSockets,
                                                closeWSConnection, getWalletWebSockets,
                                                getWalletWebSockets, initWSConnection,
                                                notify, runWalletWS, upgradeApplicationWS)
import           Pos.Wallet.Web.State          (AccountLookupMode (..), WalletWebDB,
                                                WebWalletModeDB, addAccount,
                                                addOnlyNewTxMeta, addUpdate, closeState,
                                                createWSet, createWallet,
                                                doesAccountExist, getHistoryCache,
                                                getNextUpdate, getProfile, getTxMeta,
                                                getWSetAddresses, getWSetMeta,
                                                getWSetPassLU, getWalletAccounts,
                                                getWalletAddresses, getWalletMeta,
                                                getWalletState, openState, removeAccount,
                                                removeNextUpdate, removeWallet,
                                                runWalletWebDB, setProfile, setWSetMeta,
                                                setWSetPassLU, setWalletMeta,
                                                setWalletTransactionMeta, testReset,
                                                updateHistoryCache)
import           Pos.Wallet.Web.Util           (deriveLvl2KeyPair)
import           Pos.Web.Server                (serveImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type WalletWebHandler m = WalletWebSockets (WalletWebDB m)

type WalletWebMode m
    = ( WalletMode WalletSscType m
      , MonadKeys m -- FIXME: Why isn't it implied by the
                    -- WalletMode constraint above?
      , WebWalletModeDB m
      , MonadDBLimits m
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
    => m (Set NodeId)
    -> SendActions m
    -> WalletWebHandler m (WalletWebHandler m :~> Handler)
    -> WalletWebHandler m (Server WalletApi)
walletServer getPeers sendActions nat = do
    ws    <- lift getWalletState
    socks <- getWalletWebSockets
    let getPeers' = lift . lift $ getPeers
    let sendActions' = hoistSendActions
            (lift . lift)
            (runWalletWebDB ws . runWalletWS socks)
            sendActions
    nat >>= launchNotifier
    addInitialRichAccount getPeers' sendActions' 0
    (`enter` servantHandlers getPeers' sendActions') <$> nat

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

    -- historyNotifier :: WalletWebMode m => m ()
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
    => m (Set NodeId)
    -> SendActions m
    -> ServerT WalletApi m
servantHandlers getPeers sendActions =
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
     apiRenameWSet
    :<|>
     apiImportKey
    :<|>
     apiChangeWSetPassphrase
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
    apiGetWSet                  = (catchWalletError . getWSet)
    apiGetWSets                 = catchWalletError getWSets
    apiNewWSet                  = (\a -> catchWalletError . newWSet a)
    apiRenameWSet               = (\a -> catchWalletError . renameWSet a)
    apiRestoreWSet              = (\a -> catchWalletError . newWSet a)
    apiImportKey                = (\a -> catchWalletError . importKey a)
    apiChangeWSetPassphrase     = (\a b -> catchWalletError . changeWSetPassphrase a b)
    apiGetWallet                = catchWalletError . getWallet
    apiGetWallets               = catchWalletError . getWallets
    apiUpdateWallet             = (\a -> catchWalletError . updateWallet a)
    apiNewWallet                = (\a -> catchWalletError . newWallet RandomSeed a)
    apiDeleteWallet             = catchWalletError . deleteWallet
    apiNewAccount               = (\a -> catchWalletError . newAccount RandomSeed a)
    apiIsValidAddress           = (\a -> catchWalletError . isValidAddress a)
    apiGetUserProfile           = catchWalletError getUserProfile
    apiUpdateUserProfile        = catchWalletError . updateUserProfile
    apiTxsPayments              = (\a b c -> catchWalletError . send getPeers sendActions a b c)
    apiTxsPaymentsExt           = (\a b c d e f -> catchWalletError . sendExtended getPeers sendActions a b c d e f)
    apiUpdateTransaction        = (\a b -> catchWalletError . updateTransaction a b)
    apiGetHistory               = (\a b -> catchWalletError . getHistory a b)
    apiSearchHistory            = (\a b c d -> catchWalletError . searchHistory a b c d)
    apiNextUpdate               = catchWalletError nextUpdate
    apiApplyUpdate              = catchWalletError applyUpdate
    apiRedeemAda                = \a -> catchWalletError . redeemAda getPeers sendActions a
    apiRedeemAdaPaperVend       = \a -> catchWalletError . redeemAdaPaperVend getPeers sendActions a
    apiReportingInitialized     = catchWalletError . reportingInitialized
    apiReportingElectroncrash   = catchWalletError . reportingElectroncrash
    apiSettingsSlotDuration     = catchWalletError (fromIntegral <$> blockchainSlotDuration)
    apiSettingsSoftwareVersion  = catchWalletError (pure curSoftwareVersion)
    apiSettingsSyncProgress     = catchWalletError syncProgress

    catchWalletError            = catchOtherError . try
    catchOtherError             = E.handleAll $ \e -> do
        logError $ sformat ("Uncaught error in wallet method: "%shown) e
        throwM e

-- getAddresses :: WalletWebMode m => m [CAddress]
-- getAddresses = map addressToCAddress <$> myAddresses

-- getBalances :: WalletWebMode m => m [(CAddress, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCAddress addr) <$> getBalance addr

getUserProfile :: WalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: WalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getAccountBalance :: WalletWebMode m => CAccountAddress -> m Coin
getAccountBalance cAccAddr =
    getBalance <=< decodeCAddressOrFail $ caaAddress cAccAddr

getAccount :: WalletWebMode m => CAccountAddress -> m CAccount
getAccount cAddr = do
    balance <- mkCCoin <$> getAccountBalance cAddr
    return $ CAccount (caaAddress cAddr) balance

getWalletAccAddrsOrThrow
    :: (WebWalletModeDB m, MonadThrow m)
    => AccountLookupMode -> CWalletAddress -> m [CAccountAddress]
getWalletAccAddrsOrThrow mode wCAddr =
    getWalletAccounts mode wCAddr >>= maybeThrow noWallet
  where
    noWallet =
        Internal $ sformat ("No wallet with address "%build%" found") wCAddr

getAccounts :: WalletWebMode m => CWalletAddress -> m [CAccount]
getAccounts = getWalletAccAddrsOrThrow Existing >=> mapM getAccount

getWallet :: WalletWebMode m => CWalletAddress -> m CWallet
getWallet cAddr = do
    accounts <- getAccounts cAddr
    meta     <- getWalletMeta cAddr >>= maybeThrow noWallet
    pure $ CWallet cAddr meta accounts
  where
    noWallet =
        Internal $ sformat ("No wallet with address "%build%" found") cAddr

getWSet :: WalletWebMode m => CAddress WS -> m CWalletSet
getWSet cAddr = do
    meta       <- getWSetMeta cAddr >>= maybeThrow noWSet
    walletsNum <- length <$> getWallets (Just cAddr)
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKByAddr cAddr
    passLU     <- getWSetPassLU cAddr >>= maybeThrow noWSet
    pure $ CWalletSet cAddr meta walletsNum hasPass passLU
  where
    noWSet = Internal $
        sformat ("No wallet set with address "%build%" found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: MonadThrow m => CAddress w -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . Internal $
            sformat ("Error while decoding CAddress: "%stext) err

getWallets :: WalletWebMode m => Maybe (CAddress WS) -> m [CWallet]
getWallets mCAddr = do
    whenJust mCAddr $ \cAddr -> getWSetMeta cAddr `whenNothingM_` noWSet cAddr
    mapM getWallet . filterByAddr mCAddr =<< getWalletAddresses
  where
    filterByAddr = maybe identity $ \cAddr ->
        filter $ (== cAddr) . cwaWSAddress
    noWSet cAddr = throwM . Internal $
        sformat ("No wallet set with address "%build%" found") cAddr

getWSets :: WalletWebMode m => m [CWalletSet]
getWSets = getWSetAddresses >>= mapM getWSet

decodeCPassPhraseOrFail
    :: WalletWebMode m => MCPassPhrase -> m PassPhrase
decodeCPassPhraseOrFail (Just cpass) =
    either (const . throwM $ Internal "Decoding of passphrase failed") return $
    cPassPhraseToPassPhrase cpass
decodeCPassPhraseOrFail Nothing = return emptyPassphrase

send
    :: (WalletWebMode m)
    => m (Set NodeId)
    -> SendActions m
    -> Maybe CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> m CTx
send getPeers sendActions cpass srcCAddr dstCAddr c =
    sendExtended getPeers sendActions cpass srcCAddr dstCAddr c ADA mempty mempty

sendExtended
    :: (WalletWebMode m)
    => m (Set NodeId)
    -> SendActions m
    -> Maybe CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> CCurrency
    -> Text
    -> Text
    -> m CTx
sendExtended getPeers sendActions cpassphrase srcWallet dstAccount coin curr title desc = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    dstAddr <- decodeCAddressOrFail dstAccount
    allAccounts <- getWalletAccAddrsOrThrow Existing srcWallet
    distr@(remaining, spendings) <- selectSrcAccounts coin allAccounts
    logDebug $ buildDistribution distr
    mRemTx <- mkRemainingTx remaining
    let txs = TxOutAux (TxOut dstAddr coin) [] :| maybe mempty one mRemTx
    srcTxOuts <- forM (toList spendings) $ \(cAddr, c) -> do
        addr <- decodeCAddressOrFail $ caaAddress cAddr
        return (TxOut addr c)
    sendDo passphrase (fst <$> spendings) txs srcTxOuts
  where
    selectSrcAccounts
        :: WalletWebMode m
        => Coin
        -> [CAccountAddress]
        -> m (Coin, NonEmpty (CAccountAddress, Coin))
    selectSrcAccounts reqCoins accounts
        | reqCoins == mkCoin 0 =
            throwM $ Internal "Spending non-positive amount of money!"
        | [] <- accounts =
            throwM . Internal $
            sformat ("Not enough money (need " %build % " more)") reqCoins
        | acc:accs <- accounts = do
            balance <- getAccountBalance acc
            if | balance == mkCoin 0 || caaAddress acc == dstAccount ->
                   selectSrcAccounts reqCoins accs
               | balance < reqCoins ->
                   do let remCoins = reqCoins `unsafeSubCoin` balance
                      ((acc, balance) :|) . toList <<$>>
                          selectSrcAccounts remCoins accs
               | otherwise ->
                   return
                       (balance `unsafeSubCoin` reqCoins, (acc, reqCoins) :| [])

    mkRemainingTx remaining
        | remaining == mkCoin 0 = return Nothing
        | otherwise = do
            remCAddr <- caAddress <$> newAccount RandomSeed cpassphrase srcWallet
            remAddr  <- decodeCAddressOrFail remCAddr
            let remTx = TxOutAux (TxOut remAddr remaining) []
            return $ Just remTx

    withSafeSigners (sk :| sks) passphrase action =
        withSafeSigner sk (return passphrase) $ \mss -> do
            ss <- mss `whenNothing` throwM (Internal "Passphrase doesn't match")
            case nonEmpty sks of
                Nothing -> action (ss :| [])
                Just sks' -> do
                    let action' = action . (ss :|) . toList
                    withSafeSigners sks' passphrase action'

    sendDo passphrase srcAccounts txs srcTxOuts = do
        na <- getPeers
        sks <- forM srcAccounts $ getSKByAccAddr passphrase
        srcAccAddrs <- forM srcAccounts $ decodeCAddressOrFail . caaAddress
        let dstAddrs = txOutAddress . toaOut <$> toList txs
        withSafeSigners sks passphrase $ \ss -> do
            let hdwSigner = NE.zip ss srcAccAddrs
            etx <- submitMTx sendActions hdwSigner (toList na) txs
            case etx of
                Left err ->
                    throwM . Internal $
                    sformat ("Cannot send transaction: " %stext) err
                Right (tx, _, _) -> do
                    logInfo $
                        sformat ("Successfully spent money from "%
                                 listF ", " addressF % " addresses on " %
                                 listF ", " addressF)
                        (toList srcAccAddrs)
                        dstAddrs
                    -- TODO: this should be removed in production
                    let txHash = hash tx
                    mapM_ removeAccount srcAccounts
                    addHistoryTx srcWallet curr title desc $
                        THEntry txHash tx srcTxOuts Nothing (toList srcAccAddrs) dstAddrs

    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

    buildDistribution (remaining, spendings) =
        let entries =
                spendings <&> \(CAccountAddress {..}, c) ->
                    F.bprint (build % ": " %build) c caaAddress
            remains = F.bprint ("Remaining: " %build) remaining
        in sformat
               ("Transaction input distribution:\n" %listF "\n" build %
                "\n" %build)
               (toList entries)
               remains

getHistory
    :: WalletWebMode m
    => CWalletAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory wAddr skip limit = do
    cAccAddrs <- getWalletAccAddrsOrThrow Ever wAddr
    accAddrs <- forM cAccAddrs (decodeCAddressOrFail . caaAddress)
    cHistory <-
        do  (minit, cachedTxs) <- transCache <$> getHistoryCache wAddr

            -- TODO: Fix type param! Global type param.
            TxHistoryAnswer {..} <- untag @WalletSscType getTxHistory accAddrs minit

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

            forM fullHistory $ addHistoryTx wAddr ADA mempty mempty
    pure (paginate cHistory, fromIntegral $ length cHistory)
  where
    paginate = take defaultLimit . drop defaultSkip
    defaultLimit = (fromIntegral $ fromMaybe 100 limit)
    defaultSkip = (fromIntegral $ fromMaybe 0 skip)
    transCache Nothing                = (Nothing, [])
    transCache (Just (hh, utxo, txs)) = (Just (hh, utxo), txs)

-- FIXME: is Word enough for length here?
searchHistory
    :: WalletWebMode m
    => CWalletAddress
    -> Text
    -> Maybe (CAddress Acc)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
searchHistory wAddr search mAccAddr skip limit =
    first (filter fits) <$> getHistory wAddr skip limit
  where
    fits ctx = txContainsTitle search ctx
            && maybe True (accRelates ctx) mAccAddr
    accRelates CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))

addHistoryTx
    :: WalletWebMode m
    => CWalletAddress
    -> CCurrency
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cAddr curr title desc wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta curr title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cAddr cId meta
    meta' <- maybe meta identity <$> getTxMeta cAddr cId
    return $ mkCTx diff wtx meta'

newAccount :: WalletWebMode m => AddrGenSeed -> Maybe CPassPhrase -> CWalletAddress -> m CAccount
newAccount addGenSeed cPassphrase cWAddr = do
    -- check wallet exists
    _ <- getWallet cWAddr

    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAccAddr <- genUniqueAccountAddress addGenSeed passphrase cWAddr
    addAccount cAccAddr
    getAccount cAccAddr

newWallet :: WalletWebMode m => AddrGenSeed -> Maybe CPassPhrase -> CWalletInit -> m CWallet
newWallet addGenSeed cPassphrase CWalletInit {..} = do
    -- check wallet set exists
    _ <- getWSet cwInitWSetId

    cAddr <- genUniqueWalletAddress addGenSeed cwInitWSetId
    createWallet cAddr cwInitMeta
    () <$ newAccount addGenSeed cPassphrase cAddr
    getWallet cAddr

createWSetSafe
    :: WalletWebMode m
    => CAddress WS -> CWalletSetMeta -> m CWalletSet
createWSetSafe cAddr wsMeta = do
    wSetExists <- isJust <$> getWSetMeta cAddr
    when wSetExists $
        throwM $ Internal "Wallet set with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWSet cAddr wsMeta curTime
    getWSet cAddr

newWSet :: WalletWebMode m => Maybe CPassPhrase -> CWalletSetInit -> m CWalletSet
newWSet cPassphrase CWalletSetInit {..} = do
    passphrase <- decodeCPassPhraseOrFail cPassphrase
    let CWalletSetMeta {..} = cwsInitMeta
    cAddr <- genSaveRootAddress passphrase cwsBackupPhrase
    createWSetSafe cAddr cwsInitMeta

updateWallet :: WalletWebMode m => CWalletAddress -> CWalletMeta -> m CWallet
updateWallet cAddr wMeta = do
    setWalletMeta cAddr wMeta
    getWallet cAddr

updateTransaction :: WalletWebMode m => CWalletAddress -> CTxId -> CTxMeta -> m ()
updateTransaction = setWalletTransactionMeta

-- TODO: to add when necessary
{-
deleteWSet :: WalletWebMode m => CAddress WS -> m ()
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
-}
deleteWallet :: WalletWebMode m => CWalletAddress -> m ()
deleteWallet = removeWallet

-- TODO: to add when necessary
-- deleteAccount :: WalletWebMode m => CAccountAddress -> m ()
-- deleteAccount = removeAccount

renameWSet :: WalletWebMode m => CAddress WS -> Text -> m CWalletSet
renameWSet addr newName = do
    meta <- getWSetMeta addr >>= maybeThrow (Internal "No such wallet set")
    setWSetMeta addr meta{ cwsName = newName }
    getWSet addr

changeWSetPassphrase
    :: WalletWebMode m
    => MCPassPhrase -> CAddress WS -> MCPassPhrase -> m ()
changeWSetPassphrase oldCPass wsAddr newCPass = do
    oldPass <- decodeCPassPhraseOrFail oldCPass
    newPass <- decodeCPassPhraseOrFail newCPass
    oldSK   <- getSKByAddr wsAddr
    newSK   <- maybeThrow badPass $ changeEncPassphrase oldPass newPass oldSK
    keyNo   <- getAddrIdx wsAddr
    modifySecretKey (fromIntegral keyNo) newSK
    setWSetPassLU wsAddr =<< liftIO getPOSIXTime
  where
    badPass = Internal "Invalid old passphrase given"

-- NOTE: later we will have `isValidAddress :: CCurrency -> CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode m => Text -> CCurrency -> m Bool
isValidAddress sAddr ADA =
    pure . either (const False) (const True) $ decodeTextAddress sAddr
isValidAddress _ _       = pure False

-- | Get last update info
nextUpdate :: WalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (Internal "No updates available")

applyUpdate :: WalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: WalletWebMode m => m (Set NodeId) -> SendActions m -> Maybe CPassPhrase -> CWalletRedeem -> m CTx
redeemAda getPeers sendActions cpassphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal getPeers sendActions cpassphrase crWalletId seedBs
  where
    invalidBase64 = throwM . Internal $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend :: WalletWebMode m => m (Set NodeId) -> SendActions m -> MCPassPhrase -> CPaperVendWalletRedeem -> m CTx
redeemAdaPaperVend getPeers sendActions cpassphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal getPeers sendActions cpassphrase pvWalletId seedDecBs
  where
    invalidBase58 = throwM . Internal $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e = throwM . Internal $ "Invalid mnemonic: " <> toText e
    decryptionFailed e = throwM . Internal $ "Decryption failed: " <> show e

redeemAdaInternal :: WalletWebMode m => m (Set NodeId) -> SendActions m -> MCPassPhrase -> CWalletAddress -> ByteString -> m CTx
redeemAdaInternal getPeers sendActions cpassphrase walletId seedBs = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    (_, redeemSK) <- maybeThrow (Internal "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    -- new redemption wallet
    _ <- getWallet walletId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstCAddr <- genUniqueAccountAddress RandomSeed passphrase walletId
    dstAddr <- decodeCAddressOrFail $ caaAddress dstCAddr
    na <- getPeers
    etx <- submitRedemptionTx sendActions redeemSK (toList na) dstAddr
    case etx of
        Left err -> throwM . Internal $ "Cannot send redemption transaction: " <> err
        Right ((tx, _, _), redeemAddress, redeemBalance) -> do
            -- add redemption transaction to the history of new wallet
            let txInputs = [TxOut redeemAddress redeemBalance]
            addHistoryTx walletId ADA "ADA redemption" ""
                (THEntry (hash tx) tx txInputs Nothing [srcAddr] [dstAddr])

reportingInitialized :: WalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs version (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall m. WalletWebMode m => CElectronCrashReport -> m ()
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

importKey
    :: WalletWebMode m
    => MCPassPhrase
    -> Text
    -> m CWalletSet
importKey cpassphrase (toString -> fp) = do
    WalletUserSecret{..} <-
        either secretReadError pure =<<
        readWalletUserSecret fp

    let key    = wusRootKey
        addr   = makePubKeyAddress $ encToPublic key
        wsAddr = addressToCAddress addr
        wsMeta = CWalletSetMeta wusWSetName
    addSecretKey key
    importedWSet <- createWSetSafe wsAddr wsMeta

    forM_ wusWallets $ \(walletIndex, walletName) -> do
        let wMeta = def{ cwName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueWalletAddress seedGen wsAddr
        createWallet cAddr wMeta

    forM_ wusAccounts $ \(walletIndex, accountIndex) -> do
        let wAddr = CWalletAddress wsAddr walletIndex
        newAccount (DeterminedSeed accountIndex) cpassphrase wAddr

    return importedWSet
  where
    secretReadError =
        throwM . Internal . sformat ("Failed to read secret: "%build)

-- This method is a temporal hack.
-- We spend money from /accounts/, which have randomly generated addresses.
-- To create account with some initial amount of money, we create wallet set
-- with @key = some genesis key@, and send half of savings to newly created
-- account.
addInitialRichAccount :: WalletWebMode m => m (Set NodeId) -> SendActions m -> Int -> m ()
addInitialRichAccount getPeers sendActions keyId =
    when isDevelopment . E.handleAll errHandler $ do
        key <- maybeThrow noKey (genesisDevSecretKeys ^? ix keyId)
        let enKey   = noPassEncrypt key
        let wsAddr  = makePubKeyAddress $ encToPublic enKey
            wsCAddr = addressToCAddress wsAddr

        let cpass  = Nothing
            wsMeta = CWalletSetMeta "Precreated wallet set full of money"
            wMeta  = def{ cwName = "Initial wallet" }

        addSecretKey enKey
        E.handleAll wSetExistsHandler $ do
            void $ createWSetSafe wsCAddr wsMeta

            let wInit = CWalletInit wMeta wsCAddr
            void $ newWallet (DeterminedSeed $ fromIntegral keyId) cpass wInit

        E.handleAll errHandler $ do
            wallets  <- getWallets (Just wsCAddr)
            let wAddr = fromMaybe (error "Init wallet should've been created along with wallet set!") $
                        head $ cwAddress <$> wallets
            accounts <- getAccounts wAddr
            accAddr  <- maybeThrow noAccount . head $ caAddress <$> accounts

            genesisBalance <- getBalance wsAddr
            accBalance <- getBalance =<< decodeCAddressOrFail accAddr
            let coinsToSend =
                    notExceeding (mkCoin 10000) accBalance $
                    applyCoinPortion (unsafeCoinPortionFromDouble 0.5) genesisBalance

            logError $ sformat ("Lil "%build) coinsToSend

            -- send some money from wallet set (corresponds to genesis address)
            -- to its account
            na          <- getPeers
            let signer   = fakeSigner key
            let dstCAddr = accAddr
            dstAddr     <- decodeCAddressOrFail dstCAddr
            let tx       = TxOutAux (TxOut dstAddr coinsToSend) []
            etx         <- submitTx sendActions signer (toList na) (one tx)
            case etx of
                Left err ->
                    throwM . Internal $ sformat ("Cannot send transaction \
                                        \for genesis account: "%stext) err
                Right _  ->
                    logDebug $ sformat ("Spent "%build%" from genesis \
                                        \address #"%int) coinsToSend keyId
  where
    notExceeding limit curBalance =
        min $ limit `unsafeSubCoin` min limit curBalance
    noKey = Internal $ sformat ("No genesis key #"%build) keyId
    noAccount = Internal "No account created with wallet creation!"
    errHandler = logError . sformat ("Creation of init account failed: "%build)
    wSetExistsHandler =
        logDebug . sformat ("Initial wallet set already exists ("%build%")")

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

myRootAddresses :: MonadKeys m => m [CAddress WS]
myRootAddresses =
    addressToCAddress . makePubKeyAddress . encToPublic <<$>> getSecretKeys

getAddrIdx :: WalletWebMode m => CAddress WS -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          Internal $ sformat ("No wallet set with address "%build%" found") addr

getSKByAddr
    :: WalletWebMode m
    => CAddress WS
    -> m EncryptedSecretKey
getSKByAddr cAddr = do
    idx <- getAddrIdx cAddr
    sks <- getSecretKeys
    let sk = sks !! idx
    return sk

getSKByAccAddr
    :: WalletWebMode m
    => PassPhrase
    -> CAccountAddress
    -> m EncryptedSecretKey
getSKByAccAddr passphrase accAddr@CAccountAddress {..} = do
    (addr, accKey) <-
        deriveAccountKeyPair passphrase (walletAddrByAccount accAddr) caaAccountIndex
    let accCAddr = addressToCAddress addr
    if accCAddr /= caaAddress
        then throwM . Internal $ "Account is contradictory!"
        else return accKey

genSaveRootAddress
    :: WalletWebMode m
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

data GenSeed a
    = DeterminedSeed a
    | RandomSeed

type AddrGenSeed = GenSeed Word32   -- with derivation index

generateUnique
    :: (MonadIO m, MonadThrow m, Integral a, Random a)
    => Text -> GenSeed a -> (a -> m b) -> (a -> b -> m Bool) -> m b
generateUnique desc RandomSeed generator fits = loop (100 :: Int)
  where
    loop 0 = throwM . Internal $
             sformat (build%": generation of unique item seems too difficult")
             desc
    loop i = do
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- fits rand value
        if bad
            then loop (i - 1)
            else return value
generateUnique desc (DeterminedSeed seed) generator fits = do
    value <- generator seed
    whenM (fits seed value) $
        throwM . Internal $ sformat (build%": this item is already taken") desc
    return value

genUniqueWalletAddress
    :: WalletWebMode m
    => AddrGenSeed
    -> CAddress WS
    -> m CWalletAddress
genUniqueWalletAddress genSeed wsCAddr =
    generateUnique "wallet generation"
                   genSeed
                   (return . CWalletAddress wsCAddr)
                   fits
  where
    fits idx addr = andM
        [ pure $ isNonHardened idx
        , isJust <$> getWalletMeta addr
        ]

genUniqueAccountAddress
    :: WalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> CWalletAddress
    -> m CAccountAddress
genUniqueAccountAddress genSeed passphrase wCAddr@CWalletAddress{..} =
    generateUnique "account generation" genSeed mkAccount fits
  where
    mkAccount caaAccountIndex =
        deriveAccountAddress passphrase wCAddr caaAccountIndex
    fits idx addr = andM
        [ pure $ isNonHardened idx
        , doesAccountExist Ever addr
        ]

deriveAccountKeyPair
    :: WalletWebMode m
    => PassPhrase
    -> CWalletAddress
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountKeyPair passphrase CWalletAddress{..} accIndex = do
    key <- getSKByAddr cwaWSAddress
    return $ deriveLvl2KeyPair passphrase key cwaIndex accIndex

deriveAccountAddress
    :: WalletWebMode m
    => PassPhrase
    -> CWalletAddress
    -> Word32
    -> m CAccountAddress
deriveAccountAddress passphrase wAddr@CWalletAddress{..} caaAccountIndex = do
    (accAddr, _) <- deriveAccountKeyPair passphrase wAddr caaAccountIndex
    let caaWSAddress   = cwaWSAddress
        caaWalletIndex = cwaIndex
        caaAddress     = addressToCAddress accAddr
    return CAccountAddress{..}

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
            _ -> Left "Expected 2 parts separated by '@'"

-- TODO: this is not used, and perhaps won't be
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
            _ -> Left "Expected 4 parts separated by '@'"

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CCurrency where
    parseUrlPiece = readEither . toString

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
