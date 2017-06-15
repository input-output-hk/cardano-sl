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

       , addInitialRichAccount
       ) where

import           Universum

import           Control.Concurrent               (forkFinally)
import           Control.Lens                     (ix, makeLenses, traversed, (.=))
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
import           Pos.DB.Class                     (MonadGState)
import           Pos.Discovery                    (getPeers)
import           Pos.Genesis                      (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState           (MonadReportingMem, rcReportServers)
import           Pos.Reporting.Methods            (sendReport, sendReportNodeNologs)
import           Pos.Txp                          (Utxo)
import           Pos.Txp.Core                     (TxAux (..), TxOut (..), TxOutAux (..))
import           Pos.Types                        (HeaderHash)
import           Pos.Util                         (maybeThrow)
import           Pos.Util.BackupPhrase            (toSeed)
import qualified Pos.Util.Modifier                as MM
import           Pos.Util.Servant                 (decodeCType, encodeCType)
import           Pos.Util.UserSecret              (readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage            (addSecretKey, deleteSecretKey,
                                                   getSecretKeys)
import           Pos.Wallet.Redirect              (WalletRedirects)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.WalletMode            (WalletMode, applyLastUpdate,
                                                   blockchainSlotDuration, connectedPeers,
                                                   getBalance, getTxHistory,
                                                   localChainDifficulty,
                                                   networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Account           (AddrGenSeed, GenSeed (..),
                                                   genSaveRootAddress,
                                                   genUniqueAccountAddress,
                                                   genUniqueAccountId, getAddrIdx,
                                                   getSKByAccAddr, getSKByAddr,
                                                   myRootAddresses)
import           Pos.Wallet.Web.Api               (WalletApi, walletApi)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CAccount (..),
                                                   CAccountId (..), CAccountInit (..),
                                                   CAccountMeta (..), CAddress (..),
                                                   CCoin, CElectronCrashReport (..), CId,
                                                   CInitialized,
                                                   CPaperVendWalletRedeem (..),
                                                   CPassPhrase (..), CProfile,
                                                   CProfile (..), CTx (..), CTxId,
                                                   CTxMeta (..), CTxs (..),
                                                   CUpdateInfo (..), CWAddressMeta (..),
                                                   CWallet (..), CWalletInit (..),
                                                   CWalletMeta (..), CWalletRedeem (..),
                                                   NotifyEvent (..), SyncProgress (..),
                                                   Wal, addressToCId, cIdToAddress,
                                                   coinFromCCoin, encToCId, mkCCoin,
                                                   mkCTxId, mkCTxs, toCUpdateInfo,
                                                   txContainsTitle, txIdToCTxId,
                                                   walletAddrMetaToAccount)
import           Pos.Wallet.Web.Error             (WalletError (..), rewrapToWalletError)
import           Pos.Wallet.Web.Secret            (WalletUserSecret (..),
                                                   mkGenesisWalletUserSecret, wusAccounts,
                                                   wusWalletName)
import           Pos.Wallet.Web.Server.Sockets    (ConnectionsVar, MonadWalletWebSockets,
                                                   WalletWebSockets, closeWSConnections,
                                                   getWalletWebSockets, initWSConnections,
                                                   notifyAll, upgradeApplicationWS)
import           Pos.Wallet.Web.State             (AccountLookupMode (..), WalletWebDB,
                                                   WebWalletModeDB, addOnlyNewTxMeta,
                                                   addRemovedAccount, addUpdate,
                                                   addWAddress, closeState, createAccount,
                                                   createWallet, getAccountMeta,
                                                   getAccountWAddresses, getHistoryCache,
                                                   getNextUpdate, getProfile, getTxMeta,
                                                   getWAddressIds, getWalletAddresses,
                                                   getWalletMeta, getWalletPassLU,
                                                   openState, removeAccount,
                                                   removeNextUpdate, removeWallet,
                                                   setAccountMeta,
                                                   setAccountTransactionMeta, setProfile,
                                                   setWalletMeta, setWalletPassLU,
                                                   testReset, totallyRemoveWAddress,
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

type WalletWebHandler m =
    WalletRedirects (
    WalletWebSockets (
    WalletWebDB (
    m
    )))

type WalletWebMode m
    = ( WalletMode m
      , WebWalletModeDB m
      , MonadGState m
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
    syncWSetsWithGStateLock @WalletSscType =<< mapM getSKByAddr =<< myRootAddresses
    nat >>= launchNotifier
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
bracketWalletWS = bracket initWS closeWSConnections
  where
    initWS = putText "walletServeImpl initWsConnection" >> initWSConnections

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
                    lift $ notifyAll $ NetworkDifficultyChanged networkDifficulty
                    spNetworkCD .= Just networkDifficulty

        localDifficulty <- localChainDifficulty
        oldLocalDifficulty <- use spLocalCD
        when (localDifficulty /= oldLocalDifficulty) $ do
            lift $ notifyAll $ LocalDifficultyChanged localDifficulty
            spLocalCD .= localDifficulty

        peers <- connectedPeers
        oldPeers <- use spPeers
        when (peers /= oldPeers) $ do
            lift $ notifyAll $ ConnectedPeersChanged peers
            spPeers .= peers

    updateNotifier = do
        cps <- waitForUpdate
        addUpdate $ toCUpdateInfo cps
        logDebug "Added update to wallet storage"
        notifyAll UpdateAvailable

    -- historyNotifier :: WalletWebMode m => m ()
    -- historyNotifier = do
    --     cAddresses <- myCIds
    --     for_ cAddresses $ \cAddress -> do
    --         -- TODO: is reading from acid RAM only (not reading from disk?)
    --         oldHistoryLength <- length . fromMaybe mempty <$> getAccountHistory cAddress
    --         newHistoryLength <- length <$> getHistory cAddress
    --         when (oldHistoryLength /= newHistoryLength) .
    --             notifyAll $ NewWalletTransaction cAddress

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

     getWallet
    :<|>
     getWallets
    :<|>
     newWallet
    :<|>
     newWallet
    :<|>
     renameWSet
    :<|>
     deleteWallet
    :<|>
     importWallet
    :<|>
     changeWalletPassphrase sendActions
    :<|>

     getAccount
    :<|>
     getAccounts
    :<|>
     updateAccount
    :<|>
     newAccount RandomSeed
    :<|>
     deleteAccount
    :<|>

     newWAddress RandomSeed
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
     searchHistoryLimited
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

-- getAddresses :: WalletWebMode m => m [CId]
-- getAddresses = map addressToCId <$> myAddresses

-- getBalances :: WalletWebMode m => m [(CId, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCId addr) <$> getBalance addr

getUserProfile :: WalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: WalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getWAddressBalance :: WalletWebMode m => CWAddressMeta -> m Coin
getWAddressBalance addr =
    getBalance <=< decodeCIdOrFail $ cwamId addr

getWAddress :: WalletWebMode m => CWAddressMeta -> m CAddress
getWAddress cAddr@CWAddressMeta {..} = do
    balance <- getWAddressBalance cAddr
    (ctxs, _) <-
        searchHistory
            Nothing
            (Just $ walletAddrMetaToAccount cAddr)  -- just to specify addrId is not enough
            (Just cwamId)
            Nothing
    let isUsed = not (null ctxs) || balance > minBound
    return $ CAddress cwamId (mkCCoin balance) isUsed

getAccountAddrsOrThrow
    :: (WebWalletModeDB m, MonadThrow m)
    => AccountLookupMode -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow mode accId =
    getAccountWAddresses mode accId >>= maybeThrow noWallet
  where
    noWallet =
        RequestError $
        sformat ("No account with address "%build%" found") accId

getAccount :: WalletWebMode m => AccountId -> m CAccount
getAccount accId = do
    encSK <- getSKByAddr (aiWSId accId)
    addrs <- getAccountAddrsOrThrow Existing accId
    modifier <- txMempoolToModifier encSK
    let insertions = map fst (MM.insertions modifier)
    let mergedAccAddrs = ordNub $ addrs ++ insertions
    mergedAccs <- mapM getWAddress mergedAccAddrs
    balance <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
               mapM getWAddressBalance mergedAccAddrs
    meta <- getAccountMeta accId >>= maybeThrow noWallet
    pure $ CAccount (encodeCType accId) meta mergedAccs balance
  where
    noWallet =
        RequestError $ sformat ("No account with address "%build%" found") accId

getWallet :: WalletWebMode m => CId Wal -> m CWallet
getWallet cAddr = do
    meta       <- getWalletMeta cAddr >>= maybeThrow noWSet
    wallets    <- getAccounts (Just cAddr)
    let walletsNum = length wallets
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCCoinOrFail . caAmount) wallets
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKByAddr cAddr
    passLU     <- getWalletPassLU cAddr >>= maybeThrow noWSet
    pure $ CWallet cAddr meta walletsNum balance hasPass passLU
  where
    noWSet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

-- TODO: probably poor naming
decodeCIdOrFail :: MonadThrow m => CId w -> m Address
decodeCIdOrFail = either wrongAddress pure . cIdToAddress
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CId: "%stext) err

-- TODO: these two could be removed if we decide to encode endpoint result
-- to CType automatically
decodeCAccountIdOrFail :: MonadThrow m => CAccountId -> m AccountId
decodeCAccountIdOrFail = either wrongAddress pure . decodeCType
  where wrongAddress err = throwM . DecodeError $
            sformat ("Error while decoding CAccountId: "%stext) err

decodeCCoinOrFail :: MonadThrow m => CCoin -> m Coin
decodeCCoinOrFail c =
    coinFromCCoin c `whenNothing` throwM (DecodeError "Wrong coin format")


getWalletAccountIds :: WalletWebMode m => CId Wal -> m [AccountId]
getWalletAccountIds cWalId = filter ((== cWalId) . aiWSId) <$> getWAddressIds

getWalletAddrs :: (WalletWebMode m, MonadThrow m) => CId Wal -> m [CId Addr]
getWalletAddrs cWalId = do
    addrs <- concatMapM (getAccountAddrsOrThrow Ever)
        =<< getWalletAccountIds cWalId
    pure $ map cwamId addrs

getAccounts :: WalletWebMode m => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    whenJust mCAddr $ \cAddr -> getWalletMeta cAddr `whenNothingM_` noWSet cAddr
    mapM getAccount =<< maybe getWAddressIds getWalletAccountIds mCAddr
  where
    noWSet cAddr = throwM . RequestError $
        sformat ("No account with address "%build%" found") cAddr

getWallets :: WalletWebMode m => m [CWallet]
getWallets = getWalletAddresses >>= mapM getWallet

send
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> m CTx
send sendActions passphrase srcAddr dstCAddr c =
    sendExtended sendActions passphrase srcAddr dstCAddr c mempty mempty

sendExtended
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> Text
    -> Text
    -> m CTx
sendExtended sa passphrase srcAccount dstAccount coin title desc =
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))
        title
        desc

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource CWAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: WalletWebMode m => MoneySource -> m [CWAddressMeta]
getMoneySourceAddresses (AddressMoneySource accAddr) = return $ one accAddr
getMoneySourceAddresses (AccountMoneySource wAddr) =
    getAccountAddrsOrThrow Existing wAddr
getMoneySourceAddresses (WalletMoneySource wid) =
    getWalletAccountIds wid >>=
    concatMapM (getMoneySourceAddresses . AccountMoneySource)

getMoneySourceAccount :: WalletWebMode m => MoneySource -> m AccountId
getMoneySourceAccount (AddressMoneySource accAddr) =
    return $ walletAddrMetaToAccount accAddr
getMoneySourceAccount (AccountMoneySource wAddr) = return wAddr
getMoneySourceAccount (WalletMoneySource wid) = do
    wAddr <- (head <$> getWalletAccountIds wid) >>= maybeThrow noWallets
    getMoneySourceAccount (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

sendMoney
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> Text
    -> Text
    -> m CTx
sendMoney sendActions passphrase moneySource dstDistr title desc = do
    allAddrs <- getMoneySourceAddresses moneySource
    let dstAccAddrsSet = S.fromList $ map fst $ toList dstDistr
        notDstAccounts = filter (\a -> not $ cwamId a `S.member` dstAccAddrsSet) allAddrs
        coins = foldr1 unsafeAddCoin $ snd <$> dstDistr
    distr@(remaining, spendings) <- selectSrcAccounts coins notDstAccounts
    logDebug $ buildDistribution distr
    mRemTx <- mkRemainingTx remaining
    txOuts <- forM dstDistr $ \(cAddr, coin) -> do
        addr <- decodeCIdOrFail cAddr
        return $ TxOutAux (TxOut addr coin) []
    let txOutsWithRem = maybe txOuts (\remTx -> remTx :| toList txOuts) mRemTx
    srcTxOuts <- forM (toList spendings) $ \(cAddr, c) -> do
        addr <- decodeCIdOrFail $ cwamId cAddr
        return (TxOut addr c)
    sendDo (fst <$> spendings) txOutsWithRem srcTxOuts
  where
    selectSrcAccounts
        :: WalletWebMode m
        => Coin
        -> [CWAddressMeta]
        -> m (Coin, NonEmpty (CWAddressMeta, Coin))
    selectSrcAccounts reqCoins accounts
        | reqCoins == mkCoin 0 =
            throwM $ RequestError "Spending non-positive amount of money!"
        | [] <- accounts =
            throwM . RequestError $
            sformat ("Not enough money (need " %build % " more)") reqCoins
        | acc:accs <- accounts = do
            balance <- getWAddressBalance acc
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
            relatedWallet <- getMoneySourceAccount moneySource
            account       <- newWAddress RandomSeed passphrase relatedWallet
            remAddr       <- decodeCIdOrFail (cadId account)
            let remTx = TxOutAux (TxOut remAddr remaining) []
            return $ Just remTx

    withSafeSigners (sk :| sks) action =
        withSafeSigner sk (return passphrase) $ \mss -> do
            ss <- maybeThrow (RequestError "Passphrase doesn't match") mss
            case nonEmpty sks of
                Nothing -> action (ss :| [])
                Just sks' -> do
                    let action' = action . (ss :|) . toList
                    withSafeSigners sks' action'

    sendDo srcAddrMetas txs srcTxOuts = do
        na <- getPeers
        sks <- forM srcAddrMetas $ getSKByAccAddr passphrase
        srcAddrs <- forM srcAddrMetas $ decodeCIdOrFail . cwamId
        let dstAddrs = txOutAddress . toaOut <$> toList txs
        withSafeSigners sks $ \ss -> do
            let hdwSigner = NE.zip ss srcAddrs
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
                        (toList srcAddrs)
                        dstAddrs
                    -- TODO: this should be removed in production
                    let txHash    = hash tx
                    -- TODO [CSM-251]: if money source is wallet, then this is not fully correct
                    srcAccount <- getMoneySourceAccount moneySource
                    ctxs <- addHistoryTx (aiWSId srcAccount) title desc $
                        THEntry txHash tx srcTxOuts Nothing (toList srcAddrs) dstAddrs
                    ctsOutgoing ctxs `whenNothing` throwM noOutgoingTx

    noOutgoingTx = InternalError "Can't report outgoing transaction"

    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

    buildDistribution (remaining, spendings) =
        let entries =
                spendings <&> \(CWAddressMeta {..}, c) ->
                    F.bprint (build % ": " %build) c cwamId
            remains = F.bprint ("Remaining: " %build) remaining
        in sformat
               ("Transaction input distribution:\n" %listF "\n" build %
                "\n" %build)
               (toList entries)
               remains

getFullWalletHistory :: WalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM decodeCIdOrFail =<< getWalletAddrs cWalId
    cHistory <- do
        (mInit, cachedTxs) <- transCache <$> getHistoryCache cWalId

        -- TODO: Fix type param! Global type param.
        TxHistoryAnswer {..} <- untag @WalletSscType getTxHistory addrs mInit

        -- Add allowed portion of result to cache
        let fullHistory = taHistory <> cachedTxs
            lenHistory = length taHistory
            cached = drop (lenHistory - taCachedNum) taHistory
        unless (null cached) $
            updateHistoryCache
                cWalId
                taLastCachedHash
                taCachedUtxo
                (cached <> cachedTxs)

        ctxs <- forM fullHistory $ addHistoryTx cWalId mempty mempty
        pure $ concatMap toList ctxs
    pure (cHistory, fromIntegral $ length cHistory)
  where
    transCache
        :: Maybe (HeaderHash, Utxo, [TxHistoryEntry])
        -> (Maybe (HeaderHash, Utxo), [TxHistoryEntry])
    transCache Nothing                = (Nothing, [])
    transCache (Just (hh, utxo, txs)) = (Just (hh, utxo), txs)

searchHistory
    :: WalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Text
    -> m ([CTx], Word)
searchHistory mCWalId mAccountId mAddrId mSearch = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWSId accId, [accId])
    accAddrs <- map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    addrs <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    first (filter (fits addrs)) <$> getFullWalletHistory cWalId
  where
    fits :: [CId Addr] -> CTx -> Bool
    fits addrs ctx =
        maybe True (containsInTitle ctx) mSearch
            && any (relatesToAddr ctx) addrs
    containsInTitle = flip txContainsTitle
    relatesToAddr CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

searchHistoryLimited
    :: WalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Text
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
searchHistoryLimited mCWalId mAccId mAddrId mSearch mSkip mLimit =
    first applySkipLimit <$> searchHistory mCWalId mAccId mAddrId mSearch
  where
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0

addHistoryTx
    :: WalletWebMode m
    => CId Wal
    -> Text
    -> Text
    -> TxHistoryEntry
    -> m CTxs
addHistoryTx cWalId title desc wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta title desc <$> liftIO getPOSIXTime
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cWalId cId meta
    meta' <- fromMaybe meta <$> getTxMeta cWalId cId
    walAddrs <- getWalletAddrs cWalId
    return $ mkCTxs diff wtx meta' walAddrs

newWAddress
    :: WalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newWAddress addGenSeed passphrase accId = do
    -- check wallet exists
    _ <- getAccount accId

    cAccAddr <- genUniqueAccountAddress addGenSeed passphrase accId
    addWAddress cAccAddr
    getWAddress cAccAddr

newAccount :: WalletWebMode m => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount addGenSeed passphrase CAccountInit {..} = do
    -- check wallet exists
    _ <- getWallet caInitWId

    cAddr <- genUniqueAccountId addGenSeed caInitWId
    createAccount cAddr caInitMeta
    () <$ newWAddress addGenSeed passphrase cAddr
    getAccount cAddr

createWalletSafe
    :: WalletWebMode m
    => CId Wal -> CWalletMeta -> m CWallet
createWalletSafe cid wsMeta = do
    wSetExists <- isJust <$> getWalletMeta cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet cid wsMeta curTime
    getWallet cid

newWallet :: WalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase CWalletInit {..} = do
    let CWalletMeta {..} = cwInitMeta
    cAddr <- genSaveRootAddress passphrase cwBackupPhrase
    wallet@CWallet{..} <- createWalletSafe cAddr cwInitMeta

    let accMeta = CAccountMeta { caName = "Initial account" }
    let accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    _ <- newAccount RandomSeed passphrase accInit
    return wallet

updateAccount :: WalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    setAccountMeta accId wMeta
    getAccount accId

updateTransaction :: WalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setAccountTransactionMeta accId txId txMeta

deleteWallet :: WalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    accounts <- getAccounts (Just wid)
    mapM_ (\acc -> deleteAccount =<< decodeCAccountIdOrFail (caId acc)) accounts
    removeWallet wid
    deleteSecretKey . fromIntegral =<< getAddrIdx wid

deleteAccount :: WalletWebMode m => AccountId -> m ()
deleteAccount = removeAccount

-- TODO: to add when necessary
-- deleteWAddress :: WalletWebMode m => CWAddressMeta -> m ()
-- deleteWAddress = removeWAddress

renameWSet :: WalletWebMode m => CId Wal -> Text -> m CWallet
renameWSet cid newName = do
    meta <- getWalletMeta cid >>= maybeThrow (RequestError "No such wallet")
    setWalletMeta cid meta{ cwName = newName }
    getWallet cid

-- | Creates account address with same derivation path for new wallet.
rederiveAccountAddress
    :: WalletWebMode m
    => EncryptedSecretKey -> PassPhrase -> CWAddressMeta -> m CWAddressMeta
rederiveAccountAddress newSK newPass CWAddressMeta{..} = do
    (accAddr, _) <- maybeThrow badPass $
        deriveLvl2KeyPair newPass newSK cwamWalletIndex cwamAccountIndex
    return CWAddressMeta
        { cwamWSId      = encToCId newSK
        , cwamId        = addressToCId accAddr
        , ..
        }
  where
    badPass = RequestError "Passphrase doesn't match"

data AccountsSnapshot = AccountsSnapshot
    { asExisting :: [CWAddressMeta]
    , asDeleted  :: [CWAddressMeta]
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

-- | Clones existing accounts of wallet with new passphrase and returns
-- list of old accounts
cloneWalletSetWithPass
    :: WalletWebMode m
    => EncryptedSecretKey
    -> PassPhrase
    -> CId Wal
    -> m (AccountsSnapshot, AccountsSnapshot)
cloneWalletSetWithPass newSK newPass wid = do
    accIds <- getWalletAccountIds wid
    fmap mconcat . forM accIds $ \accId@AccountId{..} -> do
        wMeta <- getAccountMeta accId >>= maybeThrow noWMeta
        setAccountMeta accId wMeta
        (oldDeleted, newDeleted) <-
            unzip <$> cloneAccounts accId Deleted addRemovedAccount
        (oldExisting, newExisting) <-
            unzip <$> cloneAccounts accId Existing addWAddress
        let oldAddrMeta =
                AccountsSnapshot
                {asExisting = oldExisting, asDeleted = oldDeleted}
            newAddrMeta =
                AccountsSnapshot
                {asExisting = newExisting, asDeleted = newDeleted}
        logDebug $
            sformat
                ("Cloned wallet accounts: "%build%"\n\t-> "%build)
                oldAddrMeta
                newAddrMeta
        return (oldAddrMeta, newAddrMeta)
  where
    noWMeta = InternalError "Can't get wallet meta (inconsistent db)"
    cloneAccounts oldAccId lookupMode addToDB = do
        accAddrs <- getAccountAddrsOrThrow lookupMode oldAccId
        forM accAddrs $ \accAddr@CWAddressMeta {..} -> do
            newAcc <- rederiveAccountAddress newSK newPass accAddr
            _ <- addToDB newAcc
            return (accAddr, newAcc)

moveMoneyToClone
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CId Wal
    -> [CWAddressMeta]
    -> [CWAddressMeta]
    -> m ()
moveMoneyToClone sa oldPass wid oldAddrMeta newAddrMeta = do
    let ms = WalletMoneySource wid
    dist <-
        forM (zip oldAddrMeta newAddrMeta) $ \(oldAcc, newAcc) ->
            (cwamId newAcc, ) <$> getWAddressBalance oldAcc
    whenNotNull dist $ \dist' ->
        unless (all ((== mkCoin 0) . snd) dist) $
        void $
        sendMoney sa oldPass ms dist' "Wallet cloning transaction" ""

changeWalletPassphrase
    :: WalletWebMode m
    => SendActions m -> CId Wal -> PassPhrase -> PassPhrase -> m ()
changeWalletPassphrase sa wid oldPass newPass = do
    oldSK   <- getSKByAddr wid
    newSK   <- maybeThrow badPass $ changeEncPassphrase oldPass newPass oldSK

    addSecretKey newSK
    oldAddrMeta <- (`E.onException` deleteSK newPass) $ do
        (oldAddrMeta, newAddrMeta) <- cloneWalletSetWithPass newSK newPass wid
            `E.onException` do
                logError "Failed to clone wallet"
        moveMoneyToClone sa oldPass wid (asExisting oldAddrMeta) (asExisting newAddrMeta)
            `E.onException` do
                logError "Money transmition to new wallet failed"
                mapM_ totallyRemoveWAddress $ asDeleted <> asExisting $ newAddrMeta
        return oldAddrMeta
    mapM_ totallyRemoveWAddress $ asDeleted <> asExisting $ oldAddrMeta
    setWalletPassLU wid =<< liftIO getPOSIXTime
    deleteSK oldPass
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeys
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)

-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

-- | Get last update info
nextUpdate :: WalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

applyUpdate :: WalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: WalletWebMode m => SendActions m -> PassPhrase -> CWalletRedeem -> m CTx
redeemAda sendActions passphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal sendActions passphrase crWalletId seedBs
  where
    invalidBase64 =
        throwM . RequestError $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CPaperVendWalletRedeem
    -> m CTx
redeemAdaPaperVend sendActions passphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal sendActions passphrase pvWalletId seedDecBs
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
    -> PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal sendActions passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- decodeCAccountIdOrFail cAccId
    -- new redemption wallet
    _ <- getAccount accId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstCAddr <- genUniqueAccountAddress RandomSeed passphrase accId
    dstAddr <- decodeCIdOrFail $ cwamId dstCAddr
    na <- getPeers
    etx <- submitRedemptionTx sendActions redeemSK (toList na) dstAddr
    case etx of
        Left err -> throwM . RequestError $
                    "Cannot send redemption transaction: " <> err
        Right (TxAux {..}, redeemAddress, redeemBalance) -> do
            -- add redemption transaction to the history of new wallet
            let txInputs = [TxOut redeemAddress redeemBalance]
            ctxs <- addHistoryTx (aiWSId accId) "ADA redemption" ""
                (THEntry (hash taTx) taTx txInputs Nothing [srcAddr] [dstAddr])
            ctsOutgoing ctxs `whenNothing` throwM noOutgoingTx
  where
   noOutgoingTx = InternalError "Can't report outgoing transaction"

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

importWallet
    :: WalletWebMode m
    => PassPhrase
    -> Text
    -> m CWallet
importWallet passphrase (toString -> fp) = do
    secret <- rewrapToWalletError $ readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWalletSet)
    importWalletSecret passphrase wSecret
  where
    noWalletSecret =
        RequestError "This key doesn't contain HD wallet info"

importWalletSecret
    :: WalletWebMode m
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret passphrase WalletUserSecret{..} = do
    let key    = _wusRootKey
        addr   = makePubKeyAddress $ encToPublic key
        wid    = addressToCId addr
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    importedWallet <- createWalletSafe wid wMeta

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        newWAddress (DeterminedSeed accountIndex) passphrase accId

    selectAccountsFromUtxoLock @WalletSscType [key]

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
addInitialRichAccount :: WalletWebMode m => Int -> m ()
addInitialRichAccount keyId =
    when isDevelopment . E.handleAll wSetExistsHandler $ do
        key <- maybeThrow noKey (genesisDevHdwSecretKeys ^? ix keyId)
        void $ importWalletSecret emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")

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

instance FromHttpApiData (CId w) where
    parseUrlPiece = fmap addressToCId . decodeTextAddress

instance FromHttpApiData CAccountId where
    parseUrlPiece = fmap CAccountId . parseUrlPiece

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase
