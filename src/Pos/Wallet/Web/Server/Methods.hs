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

       , bracketWalletWebDB
       , bracketWalletWS
       ) where

import           Universum

import           Control.Concurrent               (forkFinally)
import           Control.Lens                     (ix, makeLenses, (.=))
import           Control.Monad.Catch              (SomeException, try)
import qualified Control.Monad.Catch              as E
import           Control.Monad.State              (runStateT)
import           Data.ByteString.Base58           (bitcoinAlphabet, decodeBase58)
import           Data.Default                     (Default (def))
import           Data.List                        (findIndex)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Set                         as S
import           Data.Tagged                      (untag)
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Data.Time.Units                  (Microsecond, Second)
import qualified Ether
import           Formatting                       (bprint, build, sformat, shown, stext,
                                                   (%))
import qualified Formatting                       as F
import           Network.Wai                      (Application)
import           Paths_cardano_sl                 (version)
import           Pos.ReportServer.Report          (ReportType (RInfo))
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Serokell.Util                    (threadDelay)
import qualified Serokell.Util.Base64             as B64
import           Serokell.Util.Text               (listJson)
import           Servant.API                      ((:<|>) ((:<|>)),
                                                   FromHttpApiData (parseUrlPiece))
import           Servant.Multipart                (fdFilePath)
import           Servant.Server                   (Handler, Server, ServerT, err403,
                                                   runHandler, serve)
import           Servant.Utils.Enter              ((:~>) (..), enter)
import           System.Wlog                      (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Client.Txp.History           (TxHistoryAnswer (..),
                                                   TxHistoryEntry (..))
import           Pos.Communication                (OutSpecs, SendActions, sendTxOuts,
                                                   submitMTx, submitRedemptionTx)
import           Pos.Constants                    (curSoftwareVersion, isDevelopment)
import           Pos.Core                         (Address (..), Coin, addressF,
                                                   decodeTextAddress, makePubKeyAddress,
                                                   makeRedeemAddress, mkCoin, sumCoins,
                                                   unsafeAddCoin, unsafeIntegerToCoin,
                                                   unsafeSubCoin)
import           Pos.Crypto                       (EncryptedSecretKey, PassPhrase,
                                                   aesDecrypt, changeEncPassphrase,
                                                   checkPassMatches, deriveAesKeyBS,
                                                   emptyPassphrase, encToPublic, hash,
                                                   redeemDeterministicKeyGen,
                                                   redeemToPublic, withSafeSigner,
                                                   withSafeSigner)
import           Pos.DB.Class                     (MonadGStateCore)
import           Pos.Discovery                    (getPeers)
import           Pos.Genesis                      (accountGenesisIndex,
                                                   genesisDevHdwSecretKeys,
                                                   walletGenesisIndex)
import           Pos.Reporting.MemState           (MonadReportingMem, rcReportServers)
import           Pos.Reporting.Methods            (sendReport, sendReportNodeNologs)
import           Pos.Txp.Core                     (TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Util                         (maybeThrow)
import           Pos.Util.BackupPhrase            (toSeed)
import qualified Pos.Util.Modifier                as MM
import           Pos.Util.UserSecret              (readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage            (MonadKeys, addSecretKey,
                                                   deleteSecretKey, getSecretKeys)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.WalletMode            (WalletMode, applyLastUpdate,
                                                   blockchainSlotDuration, connectedPeers,
                                                   getBalance, getTxHistory,
                                                   localChainDifficulty,
                                                   networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Account           (AddrGenSeed, GenSeed (..),
                                                   genSaveRootAddress,
                                                   genUniqueAccountAddress,
                                                   genUniqueWalletAddress, getAddrIdx,
                                                   getSKByAccAddr, getSKByAddr,
                                                   myRootAddresses)
import           Pos.Wallet.Web.Api               (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes       (Acc, CAccount (..),
                                                   CAccountAddress (..), CAddress, CCoin,
                                                   CElectronCrashReport (..),
                                                   CInitialized,
                                                   CPaperVendWalletRedeem (..),
                                                   CPassPhrase (..), CProfile,
                                                   CProfile (..), CTx (..), CTxId,
                                                   CTxMeta (..), CUpdateInfo (..),
                                                   CWallet (..), CWalletAddress (..),
                                                   CWalletInit (..), CWalletMeta (..),
                                                   CWalletRedeem (..), CWalletSet (..),
                                                   CWalletSetInit (..),
                                                   CWalletSetMeta (..), MCPassPhrase,
                                                   NotifyEvent (..), SyncProgress (..),
                                                   WS, WalletAddress (..),
                                                   addressToCAddress, cAddressToAddress,
                                                   cPassPhraseToPassPhrase, coinFromCCoin,
                                                   encToCAddress, fromCWalletAddress,
                                                   mkCCoin, mkCTx, mkCTxId, toCUpdateInfo,
                                                   toCWalletAddress, txContainsTitle,
                                                   txIdToCTxId, walletAddrByAccount)
import           Pos.Wallet.Web.Error             (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Secret            (WalletUserSecret (..))
import           Pos.Wallet.Web.Server.Sockets    (ConnectionsVar, MonadWalletWebSockets,
                                                   WalletWebSockets, closeWSConnection,
                                                   getWalletWebSockets,
                                                   getWalletWebSockets, initWSConnection,
                                                   notify, upgradeApplicationWS)
import           Pos.Wallet.Web.State             (AccountLookupMode (..), WalletWebDB,
                                                   WebWalletModeDB, addAccount,
                                                   addOnlyNewTxMeta, addRemovedAccount,
                                                   addUpdate, closeState, createWSet,
                                                   createWallet, getHistoryCache,
                                                   getNextUpdate, getProfile, getTxMeta,
                                                   getWSetAddresses, getWSetMeta,
                                                   getWSetPassLU, getWalletAccounts,
                                                   getWalletAddresses, getWalletMeta,
                                                   openState, removeAccount,
                                                   removeNextUpdate, removeWSet,
                                                   removeWallet, setProfile, setWSetMeta,
                                                   setWalletMeta,
                                                   setWalletTransactionMeta, testReset,
                                                   totallyRemoveAccount,
                                                   updateHistoryCache)
import           Pos.Wallet.Web.State.Storage     (WalletStorage)
import           Pos.Wallet.Web.Tracking          (BlockLockMode, MonadWalletTracking,
                                                   selectAccountsFromUtxoLock,
                                                   syncWSetsWithGStateLock,
                                                   txMempoolToModifier)
import           Pos.Wallet.Web.Util              (deriveLvl2KeyPair)
import           Pos.Web.Server                   (serveImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

type WalletWebHandler m = WalletWebSockets (WalletWebDB m)

type WalletWebMode m
    = ( WalletMode m
      , MonadKeys m -- FIXME: Why isn't it implied by the
                    -- WalletMode constraint above?
      , WebWalletModeDB m
      , MonadGStateCore m
      , MonadWalletWebSockets m
      , MonadReportingMem m
      , MonadWalletTracking m
      , BlockLockMode WalletSscType m
      )

makeLenses ''SyncProgress

walletServeImpl
    :: ( MonadIO m
       , MonadMask m
       , WalletWebMode (WalletWebHandler m))
    => WalletWebHandler m Application     -- ^ Application getter
    -> Word16                             -- ^ Port to listen
    -> WalletWebHandler m ()
walletServeImpl app port =
    serveImpl app "127.0.0.1" port

walletApplication
    :: WalletWebMode m
    => m (Server WalletApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS wsConn . serve walletApi <$> serv

walletServer
    :: (MonadIO m, WalletWebMode (WalletWebHandler m))
    => SendActions (WalletWebHandler m)
    -> WalletWebHandler m (WalletWebHandler m :~> Handler)
    -> WalletWebHandler m (Server WalletApi)
walletServer sendActions nat = do
    nat >>= launchNotifier
    addInitialRichAccount 0
    syncWSetsWithGStateLock @WalletSscType =<< mapM getSKByAddr =<< myRootAddresses
    (`enter` servantHandlers sendActions) <$> nat

bracketWalletWebDB
    :: ( MonadIO m
       , MonadMask m
       )
    => FilePath  -- ^ Path to wallet acid-state
    -> Bool      -- ^ Rebuild flag for acid-state
    -> (ExtendedState WalletStorage -> m a)
    -> m a
bracketWalletWebDB daedalusDbPath dbRebuild =
    bracket (openState dbRebuild daedalusDbPath)
            closeState

bracketWalletWS
    :: ( MonadIO m
       , MonadMask m
       )
    => (ConnectionsVar -> m a)
    -> m a
bracketWalletWS = bracket initWS closeWSConnection
  where
    initWS = putText "walletServeImpl initWsConnection" >> initWSConnection

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
    => SendActions m
    -> ServerT WalletApi m
servantHandlers sendActions =
     testResetAll
    :<|>

     getWSet
    :<|>
     getWSets
    :<|>
     newWSet
    :<|>
     newWSet
    :<|>
     renameWSet
    :<|>
     deleteWSet
    :<|>
     importWSet
    :<|>
     changeWSetPassphrase sendActions
    :<|>

     getWallet
    :<|>
     getWallets
    :<|>
     updateWallet
    :<|>
     newWallet RandomSeed
    :<|>
     deleteWallet
    :<|>

     newAccount RandomSeed
    :<|>

     isValidAddress
    :<|>

     getUserProfile
    :<|>
     updateUserProfile
    :<|>

     send sendActions
    :<|>
     sendExtended sendActions
    :<|>
     updateTransaction
    :<|>
     getHistory
    :<|>
     searchHistory
    :<|>

     nextUpdate
    :<|>
     applyUpdate
    :<|>

     redeemAda sendActions
    :<|>
     redeemAdaPaperVend sendActions
    :<|>

     reportingInitialized
    :<|>
     reportingElectroncrash
    :<|>

     (blockchainSlotDuration <&> fromIntegral)
    :<|>
     pure curSoftwareVersion
    :<|>
     syncProgress

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
    getBalance <=< decodeCAddressOrFail $ caaId cAccAddr

getAccount :: WalletWebMode m => CAccountAddress -> m CAccount
getAccount cAddr = do
    balance <- mkCCoin <$> getAccountBalance cAddr
    return $ CAccount (caaId cAddr) balance

getWalletAccAddrsOrThrow
    :: (WebWalletModeDB m, MonadThrow m)
    => AccountLookupMode -> CWalletAddress -> m [CAccountAddress]
getWalletAccAddrsOrThrow mode wCAddr =
    decodeCWalletAddressOrFail wCAddr >>= getWalletAccounts mode >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No wallet with address " %build % " found") wCAddr

getWallet :: WalletWebMode m => CWalletAddress -> m CWallet
getWallet cAddr = do
    addr <- decodeCWalletAddressOrFail  cAddr
    encSK <- getSKByAddr (waWSId addr)
    accAddrs <- getWalletAccAddrsOrThrow Existing cAddr
    modifier <- txMempoolToModifier encSK
    let insertions = map fst (MM.insertions modifier)
    let modAccs = S.fromList $ insertions ++ MM.deletions modifier
    let filteredAccs = filter (`S.notMember` modAccs) accAddrs
    let mergedAccAddrs = filteredAccs ++ insertions
    mergedAccs <- mapM getAccount mergedAccAddrs
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM getAccountBalance mergedAccAddrs
    meta <- getWalletMeta addr >>= maybeThrow noWallet
    pure $ CWallet cAddr meta mergedAccs balance

  where
    noWallet =
        RequestError $ sformat ("No wallet with address "%build%" found") cAddr

getWSet :: WalletWebMode m => CAddress WS -> m CWalletSet
getWSet cAddr = do
    meta       <- getWSetMeta cAddr >>= maybeThrow noWSet
    wallets    <- getWallets (Just cAddr)
    let walletsNum = length wallets
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCCoinOrFail . cwAmount) wallets
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKByAddr cAddr
    passLU     <- getWSetPassLU cAddr >>= maybeThrow noWSet
    pure $ CWalletSet cAddr meta walletsNum balance hasPass passLU
  where
    noWSet = RequestError $
        sformat ("No wallet set with address "%build%" found") cAddr

-- TODO: probably poor naming
decodeCAddressOrFail :: MonadThrow m => CAddress w -> m Address
decodeCAddressOrFail = either wrongAddress pure . cAddressToAddress
  where wrongAddress err = throwM . RequestError $
            sformat ("Error while decoding CAddress: "%stext) err


decodeCCoinOrFail :: MonadThrow m => CCoin -> m Coin
decodeCCoinOrFail c =
    coinFromCCoin c `whenNothing` throwM (RequestError "Wrong coin format")

decodeCWalletAddressOrFail :: MonadThrow m => CWalletAddress -> m WalletAddress
decodeCWalletAddressOrFail = either wrongWallet pure . fromCWalletAddress
  where wrongWallet err = throwM . RequestError $
            sformat ("Error while decoding CWalletAddress: "%stext) err

getWSetWalletAddrs :: WalletWebMode m => CAddress WS -> m [CWalletAddress]
getWSetWalletAddrs wSet = map toCWalletAddress . filter ((== wSet) . waWSId) <$> getWalletAddresses

getWallets :: WalletWebMode m => Maybe (CAddress WS) -> m [CWallet]
getWallets mCAddr = do
    whenJust mCAddr $ \cAddr -> getWSetMeta cAddr `whenNothingM_` noWSet cAddr
    mapM getWallet =<< maybe (map toCWalletAddress <$> getWalletAddresses) getWSetWalletAddrs mCAddr
  where
    noWSet cAddr = throwM . RequestError $
        sformat ("No wallet set with address "%build%" found") cAddr

getWSets :: WalletWebMode m => m [CWalletSet]
getWSets = getWSetAddresses >>= mapM getWSet

decodeCPassPhraseOrFail
    :: WalletWebMode m => MCPassPhrase -> m PassPhrase
decodeCPassPhraseOrFail (Just cpass) =
    either (\_ -> throwM $ RequestError "Decoding of passphrase failed") return $
    cPassPhraseToPassPhrase cpass
decodeCPassPhraseOrFail Nothing = return emptyPassphrase

send
    :: (WalletWebMode m)
    => SendActions m
    -> Maybe CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> m CTx
send sendActions cpass srcCAddr dstCAddr c =
    sendExtended sendActions cpass srcCAddr dstCAddr c mempty mempty

sendExtended
    :: WalletWebMode m
    => SendActions m
    -> Maybe CPassPhrase
    -> CWalletAddress
    -> CAddress Acc
    -> Coin
    -> Text
    -> Text
    -> m CTx
sendExtended sa cpassphrase srcWallet dstAccount coin title desc =
    sendMoney
        sa
        cpassphrase
        (WalletMoneySource srcWallet)
        (one (dstAccount, coin))
        title
        desc

data MoneySource
    = WalletSetMoneySource (CAddress WS)
    | WalletMoneySource CWalletAddress
    | AccountMoneySource CAccountAddress
    deriving (Show, Eq)

getMoneySourceAccounts :: WalletWebMode m => MoneySource -> m [CAccountAddress]
getMoneySourceAccounts (AccountMoneySource accAddr) = return $ one accAddr
getMoneySourceAccounts (WalletMoneySource wAddr) =
    getWalletAccAddrsOrThrow Existing wAddr
getMoneySourceAccounts (WalletSetMoneySource wsAddr) =
    getWSetWalletAddrs wsAddr >>=
    concatMapM (getMoneySourceAccounts . WalletMoneySource)

getMoneySourceWallet :: WalletWebMode m => MoneySource -> m CWalletAddress
getMoneySourceWallet (AccountMoneySource accAddr)  =
    return $ toCWalletAddress $ walletAddrByAccount accAddr
getMoneySourceWallet (WalletMoneySource wAddr)     = return wAddr
getMoneySourceWallet (WalletSetMoneySource wsAddr) = do
    wAddr <- (head <$> getWSetWalletAddrs wsAddr) >>= maybeThrow noWallets
    getMoneySourceWallet (WalletMoneySource wAddr)
  where
    noWallets = InternalError "Wallet set has no wallets"

sendMoney
    :: WalletWebMode m
    => SendActions m
    -> Maybe CPassPhrase
    -> MoneySource
    -> NonEmpty (CAddress Acc, Coin)
    -> Text
    -> Text
    -> m CTx
sendMoney sendActions cpassphrase moneySource dstDistr title desc = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    allAccounts <- getMoneySourceAccounts moneySource
    let dstAccAddrsSet = S.fromList $ map fst $ toList dstDistr
        notDstAccounts = filter (\a -> not $ caaId a `S.member` dstAccAddrsSet) allAccounts
        coins = foldr1 unsafeAddCoin $ snd <$> dstDistr
    distr@(remaining, spendings) <- selectSrcAccounts coins notDstAccounts
    logDebug $ buildDistribution distr
    mRemTx <- mkRemainingTx remaining
    txOuts <- forM dstDistr $ \(cAddr, coin) -> do
        addr <- decodeCAddressOrFail cAddr
        return $ TxOutAux (TxOut addr coin) []
    let txOutsWithRem = maybe txOuts (\remTx -> remTx :| toList txOuts) mRemTx
    srcTxOuts <- forM (toList spendings) $ \(cAddr, c) -> do
        addr <- decodeCAddressOrFail $ caaId cAddr
        return (TxOut addr c)
    sendDo passphrase (fst <$> spendings) txOutsWithRem srcTxOuts
  where
    selectSrcAccounts
        :: WalletWebMode m
        => Coin
        -> [CAccountAddress]
        -> m (Coin, NonEmpty (CAccountAddress, Coin))
    selectSrcAccounts reqCoins accounts
        | reqCoins == mkCoin 0 =
            throwM $ RequestError "Spending non-positive amount of money!"
        | [] <- accounts =
            throwM . RequestError $
            sformat ("Not enough money (need " %build % " more)") reqCoins
        | acc:accs <- accounts = do
            balance <- getAccountBalance acc
            if | balance == mkCoin 0 ->
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
            relatedWallet <- getMoneySourceWallet moneySource
            account       <- newAccount RandomSeed cpassphrase relatedWallet
            remAddr       <- decodeCAddressOrFail (caId account)
            let remTx = TxOutAux (TxOut remAddr remaining) []
            return $ Just remTx

    withSafeSigners (sk :| sks) passphrase action =
        withSafeSigner sk (return passphrase) $ \mss -> do
            ss <- maybeThrow (RequestError "Passphrase doesn't match") mss
            case nonEmpty sks of
                Nothing -> action (ss :| [])
                Just sks' -> do
                    let action' = action . (ss :|) . toList
                    withSafeSigners sks' passphrase action'

    sendDo passphrase srcAccounts txs srcTxOuts = do
        na <- getPeers
        sks <- forM srcAccounts $ getSKByAccAddr passphrase
        srcAccAddrs <- forM srcAccounts $ decodeCAddressOrFail . caaId
        let dstAddrs = txOutAddress . toaOut <$> toList txs
        withSafeSigners sks passphrase $ \ss -> do
            let hdwSigner = NE.zip ss srcAccAddrs
            etx <- submitMTx sendActions hdwSigner (toList na) txs
            case etx of
                Left err ->
                    throwM . RequestError $
                    sformat ("Cannot send transaction: " %stext) err
                Right (TxAux {taTx = tx}) -> do
                    logInfo $
                        sformat ("Successfully spent money from "%
                                 listF ", " addressF % " addresses on " %
                                 listF ", " addressF)
                        (toList srcAccAddrs)
                        dstAddrs
                    -- TODO: this should be removed in production
                    let txHash    = hash tx
                    -- TODO [CSM-251]: if money source is wallet set, then this is not fully correct
                    srcWallet <- getMoneySourceWallet moneySource
                    mapM_ removeAccount srcAccounts
                    addHistoryTx srcWallet title desc $
                        THEntry txHash tx srcTxOuts Nothing (toList srcAccAddrs) dstAddrs

    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

    buildDistribution (remaining, spendings) =
        let entries =
                spendings <&> \(CAccountAddress {..}, c) ->
                    F.bprint (build % ": " %build) c caaId
            remains = F.bprint ("Remaining: " %build) remaining
        in sformat
               ("Transaction input distribution:\n" %listF "\n" build %
                "\n" %build)
               (toList entries)
               remains

getHistory
    :: WalletWebMode m
    => CWalletAddress -> Maybe Word -> Maybe Word -> m ([CTx], Word)
getHistory cWAddr skip limit = do
    wAddr <- decodeCWalletAddressOrFail cWAddr
    cAccAddrs <- getWalletAccAddrsOrThrow Ever cWAddr
    accAddrs <- forM cAccAddrs (decodeCAddressOrFail . caaId)
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

            forM fullHistory $ addHistoryTx cWAddr mempty mempty
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
searchHistory cWAddr search mAccAddr skip limit = do
    first (filter fits) <$> getHistory cWAddr skip limit
  where
    fits ctx = txContainsTitle search ctx
            && maybe True (accRelates ctx) mAccAddr
    accRelates CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))

addHistoryTx
    :: WalletWebMode m
    => CWalletAddress
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTx
addHistoryTx cWAddr title desc wtx@THEntry{..} = do
    wAddr <- decodeCWalletAddressOrFail cWAddr
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta wAddr cId meta
    meta' <- fromMaybe meta <$> getTxMeta wAddr cId
    return $ mkCTx diff wtx meta'


newAccount
    :: WalletWebMode m
    => AddrGenSeed
    -> MCPassPhrase
    -> CWalletAddress
    -> m CAccount
newAccount addGenSeed cPassphrase cWAddr = do
    -- check wallet exists
    wAddr <- decodeCWalletAddressOrFail cWAddr
    _ <- getWallet cWAddr

    passphrase <- decodeCPassPhraseOrFail cPassphrase
    cAccAddr <- genUniqueAccountAddress addGenSeed passphrase wAddr
    addAccount cAccAddr
    getAccount cAccAddr

newWallet :: WalletWebMode m => AddrGenSeed -> MCPassPhrase -> CWalletInit -> m CWallet
newWallet addGenSeed cPassphrase CWalletInit {..} = do
    -- check wallet set exists
    _ <- getWSet cwInitWSetId

    cAddr <- genUniqueWalletAddress addGenSeed cwInitWSetId
    createWallet cAddr cwInitMeta
    let cWAddr = toCWalletAddress cAddr
    () <$ newAccount addGenSeed cPassphrase cWAddr
    getWallet cWAddr

createWSetSafe
    :: WalletWebMode m
    => CAddress WS -> CWalletSetMeta -> m CWalletSet
createWSetSafe cAddr wsMeta = do
    wSetExists <- isJust <$> getWSetMeta cAddr
    when wSetExists $
        throwM $ RequestError "Wallet set with that mnemonics already exists"
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
updateWallet cWAddr wMeta = do
    wAddr <- decodeCWalletAddressOrFail cWAddr
    setWalletMeta wAddr wMeta
    getWallet cWAddr

updateTransaction :: WalletWebMode m => CWalletAddress -> CTxId -> CTxMeta -> m ()
updateTransaction cWAddr txId txMeta = do
    wAddr <- decodeCWalletAddressOrFail cWAddr
    setWalletTransactionMeta wAddr txId txMeta

deleteWSet :: WalletWebMode m => CAddress WS -> m ()
deleteWSet wsAddr = do
    wallets <- getWallets (Just wsAddr)
    mapM_ (deleteWallet . cwId) wallets
    removeWSet wsAddr
    deleteSecretKey . fromIntegral =<< getAddrIdx wsAddr

deleteWallet :: WalletWebMode m => CWalletAddress -> m ()
deleteWallet = decodeCWalletAddressOrFail >=> removeWallet

-- TODO: to add when necessary
-- deleteAccount :: WalletWebMode m => CAccountAddress -> m ()
-- deleteAccount = removeAccount

renameWSet :: WalletWebMode m => CAddress WS -> Text -> m CWalletSet
renameWSet addr newName = do
    meta <- getWSetMeta addr >>= maybeThrow (RequestError "No such wallet set")
    setWSetMeta addr meta{ cwsName = newName }
    getWSet addr

-- | Creates account address with same derivation path for new wallet set.
rederiveAccountAddress
    :: WalletWebMode m
    => EncryptedSecretKey -> MCPassPhrase -> CAccountAddress -> m CAccountAddress
rederiveAccountAddress newSK newCPass CAccountAddress{..} = do
    newPass <- decodeCPassPhraseOrFail newCPass
    (accAddr, _) <- maybeThrow badPass $
        deriveLvl2KeyPair newPass newSK caaWalletIndex caaAccountIndex
    return CAccountAddress
        { caaWSId      = encToCAddress newSK
        , caaId        = addressToCAddress accAddr
        , ..
        }
  where
    badPass = RequestError "Passphrase doesn't match"

data AccountsSnapshot = AccountsSnapshot
    { asExisting :: [CAccountAddress]
    , asDeleted  :: [CAccountAddress]
    }

instance Monoid AccountsSnapshot where
    mempty = AccountsSnapshot mempty mempty
    AccountsSnapshot a1 b1 `mappend` AccountsSnapshot a2 b2 =
        AccountsSnapshot (a1 <> a2) (b1 <> b2)

instance Buildable AccountsSnapshot where
    build AccountsSnapshot {..} =
        bprint
            ("{ existing: "%listJson% ", deleted: "%listJson)
            asExisting
            asDeleted

-- | Clones existing accounts of wallet set with new passphrase and returns
-- list of old accounts
cloneWalletSetWithPass
    :: WalletWebMode m
    => EncryptedSecretKey
    -> MCPassPhrase
    -> CAddress WS
    -> m (AccountsSnapshot, AccountsSnapshot)
cloneWalletSetWithPass newSK newPass wsAddr = do
    wAddrs <- getWSetWalletAddrs wsAddr
    fmap mconcat . forM wAddrs $ \cWAddr -> do
        wAddr@WalletAddress {..} <- decodeCWalletAddressOrFail cWAddr
        wMeta <- getWalletMeta wAddr >>= maybeThrow noWMeta
        setWalletMeta wAddr wMeta
        (oldDeleted, newDeleted) <-
            unzip <$> cloneAccounts cWAddr Deleted addRemovedAccount
        (oldExisting, newExisting) <-
            unzip <$> cloneAccounts cWAddr Existing addAccount
        let oldAccs =
                AccountsSnapshot
                {asExisting = oldExisting, asDeleted = oldDeleted}
            newAccs =
                AccountsSnapshot
                {asExisting = newExisting, asDeleted = newDeleted}
        logDebug $
            sformat
                ("Cloned wallet set accounts: "%build%"\n\t-> "%build)
                oldAccs
                newAccs
        return (oldAccs, newAccs)
  where
    noWMeta = InternalError "Can't get wallet meta (inconsistent db)"
    cloneAccounts oldWAddr lookupMode addToDB = do
        accAddrs <- getWalletAccAddrsOrThrow lookupMode oldWAddr
        forM accAddrs $ \accAddr@CAccountAddress {..} -> do
            newAcc <- rederiveAccountAddress newSK newPass accAddr
            _ <- addToDB newAcc
            return (accAddr, newAcc)

moveMoneyToClone
    :: WalletWebMode m
    => SendActions m
    -> MCPassPhrase
    -> CAddress WS
    -> [CAccountAddress]
    -> [CAccountAddress]
    -> m ()
moveMoneyToClone sa oldPass wsAddr oldAccs newAccs = do
    let ms = WalletSetMoneySource wsAddr
    dist <-
        forM (zip oldAccs newAccs) $ \(oldAcc, newAcc) ->
            (caaId newAcc, ) <$> getAccountBalance oldAcc
    whenNotNull dist $ \dist' ->
        unless (all ((== mkCoin 0) . snd) dist) $
        void $
        sendMoney sa oldPass ms dist' "Wallet set cloning transaction" ""

changeWSetPassphrase
    :: WalletWebMode m
    => SendActions m -> CAddress WS -> MCPassPhrase -> MCPassPhrase -> m ()
changeWSetPassphrase sa wsAddr oldCPass newCPass = do
    oldPass <- decodeCPassPhraseOrFail oldCPass
    newPass <- decodeCPassPhraseOrFail newCPass
    oldSK   <- getSKByAddr wsAddr
    newSK   <- maybeThrow badPass $ changeEncPassphrase oldPass newPass oldSK

    -- TODO [CSM-236]: test on oldWSAddr == newWSAddr
    addSecretKey newSK
    oldAccs <- (`E.onException` deleteSK newPass) $ do
        (oldAccs, newAccs) <- cloneWalletSetWithPass newSK newCPass wsAddr
            `E.onException` do
                logError "Failed to clone walletset"
        moveMoneyToClone sa oldCPass wsAddr (asExisting oldAccs) (asExisting newAccs)
            `E.onException`
            logError "Money transmition failed in progress \
                      \everything is bad"  -- TODO: rollback ?
        return oldAccs
    mapM_ totallyRemoveAccount $ asDeleted <> asExisting $ oldAccs
    deleteSK oldPass
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCAddress k == wsAddr && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeys
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)

-- NOTE: later we will have `isValidAddress :: CAddress -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

-- | Get last update info
nextUpdate :: WalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

applyUpdate :: WalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: WalletWebMode m => SendActions m -> Maybe CPassPhrase -> CWalletRedeem -> m CTx
redeemAda sendActions cpassphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal sendActions cpassphrase crWalletId seedBs
  where
    invalidBase64 =
        throwM . RequestError $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend
    :: WalletWebMode m
    => SendActions m
    -> MCPassPhrase
    -> CPaperVendWalletRedeem
    -> m CTx
redeemAdaPaperVend sendActions cpassphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal sendActions cpassphrase pvWalletId seedDecBs
  where
    invalidBase58 =
        throwM . RequestError $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e =
        throwM . RequestError $ "Invalid mnemonic: " <> toText e
    decryptionFailed e =
        throwM . RequestError $ "Decryption failed: " <> show e

redeemAdaInternal
    :: WalletWebMode m
    => SendActions m
    -> MCPassPhrase
    -> CWalletAddress
    -> ByteString
    -> m CTx
redeemAdaInternal sendActions cpassphrase cWAddr seedBs = do
    passphrase <- decodeCPassPhraseOrFail cpassphrase
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    walletId <- decodeCWalletAddressOrFail cWAddr
    -- new redemption wallet
    _ <- getWallet cWAddr

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstCAddr <- genUniqueAccountAddress RandomSeed passphrase walletId
    dstAddr <- decodeCAddressOrFail $ caaId dstCAddr
    na <- getPeers
    etx <- submitRedemptionTx sendActions redeemSK (toList na) dstAddr
    case etx of
        Left err -> throwM . RequestError $
                    "Cannot send redemption transaction: " <> err
        Right (TxAux {..}, redeemAddress, redeemBalance) -> do
            -- add redemption transaction to the history of new wallet
            let txInputs = [TxOut redeemAddress redeemBalance]
            addHistoryTx cWAddr "ADA redemption" ""
                (THEntry (hash taTx) taTx txInputs Nothing [srcAddr] [dstAddr])

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

importWSet
    :: WalletWebMode m
    => MCPassPhrase
    -> Text
    -> m CWalletSet
importWSet cpassphrase (toString -> fp) = do
    secret <- rewrapToWalletError $ readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWalletSet)
    importWSetSecret cpassphrase wSecret
  where
    noWalletSecret =
        RequestError "This key doesn't contain HD wallet info"

importWSetSecret
    :: WalletWebMode m
    => MCPassPhrase
    -> WalletUserSecret
    -> m CWalletSet
importWSetSecret cpassphrase WalletUserSecret{..} = do
    let key    = wusRootKey
        addr   = makePubKeyAddress $ encToPublic key
        wsAddr = addressToCAddress addr
        wsMeta = def { cwsName = wusWSetName }
    addSecretKey key
    importedWSet <- createWSetSafe wsAddr wsMeta

    for_ wusWallets $ \(walletIndex, walletName) -> do
        let wMeta = def{ cwName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueWalletAddress seedGen wsAddr
        createWallet cAddr wMeta

    for_ wusAccounts $ \(walletIndex, accountIndex) -> do
        let cWAddr = toCWalletAddress $ WalletAddress wsAddr walletIndex
        newAccount (DeterminedSeed accountIndex) cpassphrase cWAddr

    selectAccountsFromUtxoLock @WalletSscType [key]

    return importedWSet

-- | Creates walletset with given genesis hd-wallet key.
addInitialRichAccount :: WalletWebMode m => Int -> m ()
addInitialRichAccount keyId =
    when isDevelopment . E.handleAll wSetExistsHandler $ do
        wusRootKey <- maybeThrow noKey (genesisDevHdwSecretKeys ^? ix keyId)
        let wusWSetName = "Precreated wallet set full of money"
            wusWallets  = [(walletGenesisIndex, "Initial wallet")]
            wusAccounts = [(walletGenesisIndex, accountGenesisIndex)]
        void $ importWSetSecret Nothing WalletUserSecret{..}
  where
    noKey = InternalError $ sformat ("No genesis key #"%build) keyId
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
    parseUrlPiece = fmap CWalletAddress . parseUrlPiece

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
