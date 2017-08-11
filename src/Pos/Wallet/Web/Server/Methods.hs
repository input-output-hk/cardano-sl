{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods where

import           Universum

import           Control.Lens                 (each, has, ix, traversed)
import           Control.Monad.Catch          (SomeException, try)
import qualified Control.Monad.Catch          as E
import qualified Data.Aeson                   as A
import           Data.ByteString.Base58       (bitcoinAlphabet, decodeBase58)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (Default (def))
import qualified Data.DList                   as DL
import qualified Data.HashMap.Strict          as HM
import qualified Data.List.NonEmpty           as NE
import qualified Data.Set                     as S
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Formatting                   (build, sformat, shown, stext, (%))
import qualified Formatting                   as F
import           Pos.ReportServer.Report      (ReportType (RInfo))
import qualified Serokell.Util.Base64         as B64
import           Servant.Multipart            (fdFilePath)
import           System.IO.Error              (isDoesNotExistError)
import           System.Wlog                  (logDebug, logError, logInfo, logWarning)

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.WalletBackup       ()
import           Pos.Binary.Class             (biSize)
import           Pos.Block.Logic.Util         (withBlkSemaphore_)
import           Pos.Client.Txp.Balances      (getOwnUtxos)
import           Pos.Client.Txp.History       (TxHistoryEntry (..))
import           Pos.Client.Txp.Util          (TxError (..), createMTx,
                                               overrideTxDistrBoot,
                                               overrideTxOutDistrBoot)
import           Pos.Communication            (SendActions (..), submitMTx,
                                               submitRedemptionTx)
import           Pos.Constants                (isDevelopment)
import           Pos.Core                     (Coin, TxFeePolicy (..), TxSizeLinear (..),
                                               addressF, bvdTxFeePolicy,
                                               calculateTxSizeLinear, decodeTextAddress,
                                               getCurrentTimestamp, getTimestamp,
                                               integerToCoin, makeRedeemAddress, mkCoin,
                                               unsafeAddCoin, unsafeSubCoin,
                                               _RedeemAddress)
import           Pos.Crypto                   (EncryptedSecretKey, PassPhrase, SafeSigner,
                                               aesDecrypt, deriveAesKeyBS,
                                               emptyPassphrase, fakeSigner, hash, keyGen,
                                               redeemDeterministicKeyGen, redeemToPublic,
                                               withSafeSigner, withSafeSigner)
import           Pos.DB.Class                 (gsAdoptedBVData)
import           Pos.Genesis                  (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState       (HasReportServers (..),
                                               HasReportingContext (..))
import           Pos.Reporting.Methods        (sendReport, sendReportNodeNologs)
import           Pos.Txp                      (TxFee (..))
import           Pos.Txp.Core                 (TxAux (..), TxOut (..), TxOutAux (..),
                                               TxOutDistribution)
import           Pos.Util                     (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase        (toSeed)
import           Pos.Util.UserSecret          (UserSecretDecodingError (..),
                                               readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage        (addSecretKey, deleteSecretKey,
                                               getSecretKeys)
import           Pos.Wallet.WalletMode        (applyLastUpdate, connectedPeers,
                                               getLocalHistory, localChainDifficulty,
                                               networkChainDifficulty)
import           Pos.Wallet.Web.Account       (GenSeed (..), MonadKeySearch (..),
                                               genSaveRootKey, genUniqueAccountId)
import           Pos.Wallet.Web.Backup        (AccountMetaBackup (..), StateBackup (..),
                                               WalletBackup (..), WalletMetaBackup (..),
                                               getStateBackup)
import           Pos.Wallet.Web.ClientTypes   (AccountId (..), Addr, CAccountId (..),
                                               CAccountInit (..), CAccountMeta (..),
                                               CAddress (..), CCoin,
                                               CElectronCrashReport (..), CId,
                                               CInitialized, CPaperVendWalletRedeem (..),
                                               CProfile, CProfile (..), CTx (..), CTxId,
                                               CTxMeta (..), CTxs (..), CUpdateInfo (..),
                                               CWAddressMeta (..), CWallet (..),
                                               CWalletInit (..), CWalletMeta (..),
                                               CWalletRedeem (..), SyncProgress (..), Wal,
                                               addrMetaToAccount, encToCId, mkCCoin,
                                               mkCTxs, txIdToCTxId)
import           Pos.Wallet.Web.Error         (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Mode          (MonadWalletWebMode)
import           Pos.Wallet.Web.Secret        (WalletUserSecret (..),
                                               mkGenesisWalletUserSecret, wusAccounts,
                                               wusWalletName)
import           Pos.Wallet.Web.State         (AddressLookupMode (Ever, Existing),
                                               addOnlyNewTxMeta, createAccount,
                                               getHistoryCache, getNextUpdate, getProfile,
                                               getTxMeta, getWalletMeta, removeNextUpdate,
                                               setProfile, setWalletSyncTip,
                                               setWalletTxMeta, testReset,
                                               updateHistoryCache)
import           Pos.Wallet.Web.Tracking      (fixingCachedAccModifier,
                                               syncWalletOnImport)
import           Pos.Wallet.Web.Util          (getWalletAccountIds, rewrapTxError)


getUserProfile :: MonadWalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile


newPayment
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> m CTx
newPayment sa passphrase srcAccount dstAccount coin =
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))

getTxFee
    :: MonadWalletWebMode m
    => AccountId
    -> CId Addr
    -> Coin
    -> m CCoin
getTxFee srcAccount dstAccount coin =
    computeTxFee
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource CWAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: MonadWalletWebMode m => MoneySource -> m [CWAddressMeta]
getMoneySourceAddresses (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses (AccountMoneySource accId) =
    L.getAccountAddrsOrThrow Existing accId
getMoneySourceAddresses (WalletMoneySource wid) =
    getWalletAccountIds wid >>=
    concatMapM (getMoneySourceAddresses . AccountMoneySource)

getSomeMoneySourceAccount :: MonadWalletWebMode m => MoneySource -> m AccountId
getSomeMoneySourceAccount (AddressMoneySource addrId) =
    return $ addrMetaToAccount addrId
getSomeMoneySourceAccount (AccountMoneySource accId) = return accId
getSomeMoneySourceAccount (WalletMoneySource wid) = do
    wAddr <- (head <$> getWalletAccountIds wid) >>= maybeThrow noWallets
    getSomeMoneySourceAccount (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

getMoneySourceWallet :: MoneySource -> CId Wal
getMoneySourceWallet (AddressMoneySource addrId) = cwamWId addrId
getMoneySourceWallet (AccountMoneySource accId)  = aiWId accId
getMoneySourceWallet (WalletMoneySource wid)     = wid

-- Read-only function.
computeTxFee
    :: MonadWalletWebMode m
    => MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m CCoin
computeTxFee moneySource dstDistr = mkCCoin <$> do
    feePolicy <- bvdTxFeePolicy <$> gsAdoptedBVData
    case feePolicy of
        TxFeePolicyUnknown w _               -> throwM $ unknownFeePolicy w
        TxFeePolicyTxSizeLinear linearPolicy -> do
            txAux <- stabilizeTxFee linearPolicy moneySource dstDistr >>= createFakeTxFromRawTx
            eitherToThrow invalidFee .
                integerToCoin .
                ceiling .
                calculateTxSizeLinear linearPolicy .
                biSize @TxAux $ txAux
  where
    invalidFee reason = InternalError ("Invalid fee: " <> reason)
    unknownFeePolicy =
        InternalError . sformat ("UnknownFeePolicy, tag: "%build)

sendMoney
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m CTx
sendMoney SendActions {..} passphrase moneySource dstDistr = do
    (spendings, outputs) <- prepareTx passphrase moneySource dstDistr
    sendDo spendings outputs
  where
    sendDo
        :: MonadWalletWebMode m
        => NonEmpty (CWAddressMeta, TxOut)
        -> NonEmpty TxOutAux
        -> m CTx
    sendDo spendings outputs = do
        let inputMetas = NE.map fst spendings
        let inpTxOuts = toList $ NE.map snd spendings
        sks <- mapM findKey inputMetas
        srcAddrs <- forM inputMetas $ L.decodeCIdOrFail . cwamId
        let dstAddrs = txOutAddress . toaOut <$> toList outputs
        withSafeSigners passphrase sks $ \ss -> do
            let hdwSigner = NE.zip ss srcAddrs
            TxAux {taTx = tx} <- rewrapTxError "Cannot send transaction" $
                submitMTx enqueueMsg hdwSigner outputs
            logInfo $
                sformat ("Successfully spent money from "%
                         listF ", " addressF % " addresses on " %
                         listF ", " addressF)
                (toList srcAddrs)
                dstAddrs
            -- TODO: this should be removed in production
            let txHash    = hash tx
                srcWallet = getMoneySourceWallet moneySource
            ts <- Just <$> getCurrentTimestamp
            ctxs <- addHistoryTx srcWallet $
                THEntry txHash tx inpTxOuts Nothing (toList srcAddrs) dstAddrs ts
            ctsOutgoing ctxs `whenNothing` throwM noOutgoingTx

    noOutgoingTx = InternalError "Can't report outgoing transaction"
    -- TODO eliminate copy-paste
    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

txToLinearFee :: MonadThrow m => TxSizeLinear -> TxAux -> m Coin
txToLinearFee linearPolicy =
    eitherToThrow invalidFee .
    integerToCoin .
    ceiling .
    calculateTxSizeLinear linearPolicy .
    biSize @TxAux
  where
    invalidFee reason = InternalError ("Invalid fee: " <> reason)

-- Returns (input addresses, output addresses, TxOut corresponding to input addresses)
-- Function creates remaing output in db.
prepareTx
    :: MonadWalletWebMode m
    => PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m (NonEmpty (CWAddressMeta, TxOut), NonEmpty TxOutAux)
prepareTx passphrase moneySource dstDistr = do
    feePolicy <- bvdTxFeePolicy <$> gsAdoptedBVData
    case feePolicy of
        TxFeePolicyUnknown w _               -> throwM $ unknownFeePolicy w
        TxFeePolicyTxSizeLinear linearPolicy -> do
            raw@TxRaw{..} <- stabilizeTxFee linearPolicy moneySource dstDistr
            logDebug $ buildDistribution raw
            mRemTx <- mkRemainingTxOut trRemaining
            let txOutsWithRem = maybe trOutputs (\remTx -> remTx :| toList trOutputs) mRemTx
            inpTxOuts <- spendings2TxOuts trSpendings
            let txInps = NE.zipWith (\(addr, _) txout -> (addr, txout)) trSpendings inpTxOuts
            pure (txInps, txOutsWithRem)
  where
    mkRemainingTxOut :: MonadWalletWebMode m => (Coin, TxOutDistribution) -> m (Maybe TxOutAux)
    mkRemainingTxOut (remaining, distr)
        | remaining == mkCoin 0 = return Nothing
        | otherwise = do
            relatedWallet <- getSomeMoneySourceAccount moneySource
            account       <- L.newAddress RandomSeed passphrase relatedWallet
            remAddr       <- L.decodeCIdOrFail (cadId account)
            pure $ Just $ TxOutAux (TxOut remAddr remaining) distr

    buildDistribution :: TxRaw -> Text
    buildDistribution TxRaw{..} =
        let entries =
                trSpendings <&> \(CWAddressMeta {..}, c) ->
                    F.bprint (build % ": " %build) c cwamId
            remains = F.bprint ("Remaining: " %build) (fst trRemaining)
        in sformat
               ("Transaction input distribution:\n" %listF "\n" build %
                "\n" %build)
               (toList entries)
               remains
    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

    unknownFeePolicy =
        InternalError . sformat ("Unknown Fee Policy, tag: "%build)

-- | Search such spendings that transaction's fee would be stable.
stabilizeTxFee
    :: forall m . MonadWalletWebMode m
    => TxSizeLinear
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m TxRaw
stabilizeTxFee linearPolicy moneySource dstDistr =
    stabilizeTxFeeDo 5 (TxFee $ mkCoin 0)
  where
    stabilizeTxFeeDo :: Int -> TxFee -> m TxRaw
    stabilizeTxFeeDo 0 _ = throwM $ TxError "Couldn't stabilize tx after 5 attempts"
    stabilizeTxFeeDo attempt efee@(TxFee expectedFee) = do
        txRaw <- prepareTxRaw moneySource dstDistr efee
        txAux <- createFakeTxFromRawTx txRaw
        txFee <- txToLinearFee linearPolicy txAux
        if expectedFee == txFee then pure txRaw
        else stabilizeTxFeeDo (attempt - 1) (TxFee txFee)

-- This datatype corresponds to raw transaction.
data TxRaw
    = TxRaw {
      trSpendings :: !(NonEmpty (CWAddressMeta, Coin))
    -- ^ Input addresses of tx and coins on them
    , trOutputs   :: !(NonEmpty TxOutAux)
    -- ^ Output addresses of tx (without remaing output)
    , trRemaining :: !(Coin, TxOutDistribution)
    -- ^ Remaing money and distribution
    }

spendings2TxOuts :: MonadThrow m => NonEmpty (CWAddressMeta, Coin) -> m (NonEmpty TxOut)
spendings2TxOuts spendings = fmap NE.fromList . forM (toList spendings) $ \(cAddr, c) -> do
    addr <- L.decodeCIdOrFail $ cwamId cAddr
    pure $ TxOut addr c

createFakeTxFromRawTx :: MonadWalletWebMode m => TxRaw -> m TxAux
createFakeTxFromRawTx TxRaw{..} = do
    let fakeAddr = txOutAddress . toaOut . NE.head $ trOutputs
    let fakeOutMB
            | fst trRemaining == mkCoin 0 = Nothing
            | otherwise                   = Just $ TxOutAux (TxOut fakeAddr (fst trRemaining)) (snd trRemaining)
    let txOutsWithRem = maybe trOutputs (\remTx -> remTx :| toList trOutputs) fakeOutMB
    -- We create fake signers instead of safe signers,
    -- because safe signer requires passphrase
    -- but we don't want to reveal our passphrase to compute fee.
    -- Fee depends on size of tx in bytes, sign of a tx has the fixed size
    -- so we can use arbitrary signer.
    srcAddrs <- NE.map txOutAddress <$> spendings2TxOuts trSpendings
    utxo <- getOwnUtxos (toList srcAddrs)
    (_, sk) <- keyGen
    let fakeSigners = NE.fromList $ replicate (length srcAddrs) (fakeSigner sk)
    let hdwSigners = NE.zip fakeSigners srcAddrs
    let txAuxEi = createMTx utxo hdwSigners txOutsWithRem
    either invalidTxEx pure txAuxEi
  where
    invalidTxEx = throwM . TxError . sformat ("Couldn't create a fake transaction, reason: "%build)

-- Functions doesn't write to db anything.
prepareTxRaw
    :: MonadWalletWebMode m
    => MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> TxFee
    -> m TxRaw
prepareTxRaw moneySource dstDistr fee = do
    forM_ dstDistr $ checkIsNotRedeem . fst
    allAddrs <- getMoneySourceAddresses moneySource
    let dstAccAddrsSet = S.fromList $ map fst $ toList dstDistr
        notDstAddrs = filter (\a -> not $ cwamId a `S.member` dstAccAddrsSet) allAddrs
        coins = foldr1 unsafeAddCoin $ snd <$> dstDistr
    balancesInps <- mapM L.getWAddressBalance notDstAddrs
    -- We want to minimise a fee of the transaction,
    -- fee depends on a size of the transaction and
    -- size depends on a number of inputs and outputs.
    -- Hence we want to minimise the number of inputs,
    -- so we should sort in descending order by amount
    -- to minimise the number of taken inputs.
    let addrWBal = reverse $ sortWith snd $ zip notDstAddrs balancesInps
    (remaining, trSpendings) <- selectSrcAddresses addrWBal coins fee

    let withDistr foo =
            either (throwM . RequestError) pure =<<
            runExceptT foo
    trOutputsPre <- forM dstDistr $ \(cAddr, coin) -> do
        addr <- L.decodeCIdOrFail cAddr
        pure $ TxOutAux (TxOut addr coin) []
    trOutputs <- withDistr $ overrideTxDistrBoot trOutputsPre
    remainingDistr <- withDistr $ overrideTxOutDistrBoot remaining []
    let trRemaining = (remaining, remainingDistr)
    pure TxRaw{..}
  where
    checkIsNotRedeem cId =
        whenM (has _RedeemAddress <$> L.decodeCIdOrFail cId) $
            throwM . RequestError $
            sformat ("Destination address can't be redeem address: "%build) cId

-- | Accept all addresses in descending order (by coins)
-- Addresses available to be source of the transaction, with their balances
-- Transaction amount
-- Approximate fee for transaction being built
-- Remainer + chosen input addresses with their balances
selectSrcAddresses
    :: MonadWalletWebMode m
    => [(CWAddressMeta, Coin)]
    -> Coin
    -> TxFee
    -> m (Coin, NonEmpty (CWAddressMeta, Coin))
selectSrcAddresses allAddrs outputCoins (TxFee fee) =
    either (throwM . RequestError) pure $
    selectSrcAddressesDo (outputCoins `unsafeAddCoin` fee) allAddrs
  where
    selectSrcAddressesDo
        :: MonadWalletWebMode m => Coin
        -> [(CWAddressMeta, Coin)]
        -> Either Text (Coin, NonEmpty (CWAddressMeta, Coin))
    selectSrcAddressesDo reqCoins addresses
        | reqCoins == mkCoin 0 =
            Left "Spending non-positive amount of money!"
        | [] <- addresses =
            Left $ sformat ("Not enough money (need " %build % " more)") reqCoins
        | (ad, balance):addrs <- addresses = do
            if | balance == mkCoin 0 ->
                   selectSrcAddressesDo reqCoins addrs
               | balance < reqCoins -> do
                   let remCoins = reqCoins `unsafeSubCoin` balance
                   ((ad, balance) :|) . toList <<$>> selectSrcAddressesDo remCoins addrs
               | otherwise ->
                   -- When balance >= reqCoins,
                   -- then lets try to find input with exactly @reqCoins@ coins,
                   -- in order to use one address instead of two.
                   maybe (Right (balance `unsafeSubCoin` reqCoins, (ad, balance) :| []))
                         (\fa -> Right (mkCoin 0, fa :| []))
                         (find ((reqCoins ==) . snd) addresses)

withSafeSigners
    :: (MonadIO m, MonadThrow m)
    => PassPhrase
    -> NonEmpty (EncryptedSecretKey)
    -> (NonEmpty SafeSigner -> m a) -> m a
withSafeSigners passphrase (sk :| sks) action =
    withSafeSigner sk (pure passphrase) $ \mss -> do
        ss <- maybeThrow (RequestError "Passphrase doesn't match") mss
        case nonEmpty sks of
            Nothing -> action (ss :| [])
            Just sks' -> do
                let action' = action . (ss :|) . toList
                withSafeSigners passphrase sks' action'

getFullWalletHistory :: MonadWalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM L.decodeCIdOrFail =<< L.getWalletAddrs Ever cWalId

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure $ DL.fromList hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    localHistory <- getLocalHistory addrs

    let fullHistory = DL.toList $ localHistory <> blockHistory
    ctxs <- forM fullHistory $ addHistoryTx cWalId
    let cHistory = concatMap toList ctxs
    pure (cHistory, fromIntegral $ length cHistory)

getHistory
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> m ([CTx], Word)
getHistory mCWalId mAccountId mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    accAddrs <- map cwamId <$> concatMapM (L.getAccountAddrsOrThrow Ever) accIds
    addrs <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    first (filter (fits addrs)) <$> getFullWalletHistory cWalId
  where
    fits :: [CId Addr] -> CTx -> Bool
    fits addrs ctx = any (relatesToAddr ctx) addrs
    relatesToAddr CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

getHistoryLimited
    :: MonadWalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit =
    first applySkipLimit <$> getHistory mCWalId mAccId mAddrId
  where
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0

addHistoryTx
    :: MonadWalletWebMode m
    => CId Wal
    -> TxHistoryEntry
    -> m CTxs
addHistoryTx cWalId wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta <$> case _thTimestamp of
      Nothing -> liftIO $ getPOSIXTime
      Just ts -> return $ fromIntegral (getTimestamp ts) / 1000000
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cWalId cId meta
    meta' <- fromMaybe meta <$> getTxMeta cWalId cId
    walAddrMetas <- L.getWalletAddrMetas Ever cWalId
    mkCTxs diff wtx meta' walAddrMetas & either (throwM . InternalError) pure


-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = 0

newWalletFromBackupPhrase
    :: MonadWalletWebMode m
    => PassPhrase -> CWalletInit -> m (EncryptedSecretKey, CId Wal)
newWalletFromBackupPhrase passphrase CWalletInit {..} = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase cwBackupPhrase
    let cAddr = encToCId skey

    CWallet{..} <- L.createWalletSafe cAddr cwInitMeta
    -- can't return this result, since balances can change

    let accMeta = CAccountMeta { caName = "Initial account" }
        accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    () <$ L.newAccount (DeterminedSeed initialAccAddrIdxs) passphrase accInit

    return (skey, cAddr)

newWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase cwInit = do
    (_, wId) <- newWalletFromBackupPhrase passphrase cwInit
    updateHistoryCache wId []
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withBlkSemaphore_ $ \tip -> tip <$ setWalletSyncTip wId tip
    L.getWallet wId

restoreWallet :: MonadWalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
restoreWallet passphrase cwInit = do
    (sk, wId) <- newWalletFromBackupPhrase passphrase cwInit
    syncWalletOnImport sk
    L.getWallet wId


updateTransaction :: MonadWalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta


-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: MonadWalletWebMode m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

-- | Get last update info
nextUpdate :: MonadWalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

applyUpdate :: MonadWalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: MonadWalletWebMode m => SendActions m -> PassPhrase -> CWalletRedeem -> m CTx
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
    :: MonadWalletWebMode m
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
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal SendActions {..} passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- L.decodeCAccountIdOrFail cAccId
    -- new redemption wallet
    _ <- fixingCachedAccModifier L.getAccount accId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstAddr <- L.decodeCIdOrFail . cadId =<<
               L.newAddress RandomSeed passphrase accId
    (TxAux {..}, redeemAddress, redeemBalance) <-
        rewrapTxError "Cannot send redemption transaction" $
        submitRedemptionTx enqueueMsg redeemSK dstAddr
    -- add redemption transaction to the history of new wallet
    let txInputs = [TxOut redeemAddress redeemBalance]
    ts <- Just <$> getCurrentTimestamp
    ctxs <- addHistoryTx (aiWId accId) $
        THEntry (hash taTx) taTx txInputs Nothing [srcAddr] [dstAddr] ts
    ctsIncoming ctxs `whenNothing` throwM noIncomingTx
  where
    noIncomingTx = InternalError "Can't report incoming transaction"

reportingInitialized :: MonadWalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall m. MonadWalletWebMode m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- view (reportingContext . reportServers)
    errors <- fmap lefts $ forM servers $ \serv ->
        try $ sendReport [fdFilePath $ cecUploadDump celcrash]
                         []
                         (RInfo $ show celcrash)
                         "daedalus"
                         (toString serv)
    whenNotNull errors $ handler . NE.head
  where
    fmt = ("Didn't manage to report electron crash "%shown%" because of exception "%shown)
    handler :: SomeException -> m ()
    handler e = logError $ sformat fmt celcrash e

importWallet
    :: MonadWalletWebMode m
    => PassPhrase
    -> Text
    -> m CWallet
importWallet passphrase (toString -> fp) = do
    secret <-
        rewrapToWalletError isDoesNotExistError noFile $
        rewrapToWalletError (\UserSecretDecodingError{} -> True) decodeFailed $
        readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWalletSet)
    wId <- cwId <$> importWalletSecret emptyPassphrase wSecret
    L.changeWalletPassphrase wId emptyPassphrase passphrase
    L.getWallet wId
  where
    noWalletSecret = RequestError "This key doesn't contain HD wallet info"
    noFile _ = RequestError "File doesn't exist"
    decodeFailed = RequestError . sformat ("Invalid secret file ("%build%")")

importWalletSecret
    :: MonadWalletWebMode m
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret passphrase WalletUserSecret{..} = do
    let key    = _wusRootKey
        wid    = encToCId key
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    importedWallet <- L.createWalletSafe wid wMeta

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        L.newAddress (DeterminedSeed accountIndex) passphrase accId

    void $ syncWalletOnImport key

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
addInitialRichAccount :: MonadWalletWebMode m => Int -> m ()
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

syncProgress :: MonadWalletWebMode m => m SyncProgress
syncProgress = do
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

testResetAll :: MonadWalletWebMode m => m ()
testResetAll = deleteAllKeys >> testReset
  where
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0
