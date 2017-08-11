{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods where

import           Universum

import           Control.Lens                   (has)
import           Control.Monad.Catch            (SomeException, try)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as S
import           Formatting                     (build, sformat, shown, (%))
import qualified Formatting                     as F
import           Pos.ReportServer.Report        (ReportType (RInfo))
import           Servant.Multipart              (fdFilePath)
import           System.Wlog                    (logDebug, logError, logInfo)

import           Pos.Aeson.ClientTypes          ()
import           Pos.Aeson.WalletBackup         ()
import           Pos.Binary.Class               (biSize)
import           Pos.Client.Txp.Balances        (getOwnUtxos)
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Client.Txp.Util            (TxError (..), createMTx,
                                                 overrideTxDistrBoot,
                                                 overrideTxOutDistrBoot)
import           Pos.Communication              (SendActions (..), submitMTx)
import           Pos.Core                       (Coin, TxFeePolicy (..),
                                                 TxSizeLinear (..), addressF,
                                                 bvdTxFeePolicy, calculateTxSizeLinear,
                                                 decodeTextAddress, getCurrentTimestamp,
                                                 integerToCoin, mkCoin, unsafeAddCoin,
                                                 unsafeSubCoin, _RedeemAddress)
import           Pos.Crypto                     (PassPhrase, fakeSigner, hash, keyGen,
                                                 withSafeSigners)
import           Pos.DB.Class                   (gsAdoptedBVData)
import           Pos.Reporting.MemState         (HasReportServers (..),
                                                 HasReportingContext (..))
import           Pos.Reporting.Methods          (sendReport, sendReportNodeNologs)
import           Pos.Txp                        (TxFee (..))
import           Pos.Txp.Core                   (TxAux (..), TxOut (..), TxOutAux (..),
                                                 TxOutDistribution)
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Wallet.KeyStorage          (deleteSecretKey, getSecretKeys)
import           Pos.Wallet.WalletMode          (applyLastUpdate, connectedPeers,
                                                 localChainDifficulty,
                                                 networkChainDifficulty)
import           Pos.Wallet.Web.Account         (GenSeed (..), MonadKeySearch (..))
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CAddress (..),
                                                 CCoin, CElectronCrashReport (..), CId,
                                                 CInitialized, CProfile, CProfile (..),
                                                 CTx (..), CTxs (..), CUpdateInfo (..),
                                                 CWAddressMeta (..), SyncProgress (..),
                                                 Wal, addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTx)
import qualified Pos.Wallet.Web.Methods.Logic   as L
import           Pos.Wallet.Web.Mode            (MonadWalletWebMode)
import           Pos.Wallet.Web.State           (AddressLookupMode (Existing),
                                                 getNextUpdate, getProfile,
                                                 removeNextUpdate, setProfile, testReset)
import           Pos.Wallet.Web.Util            (getWalletAccountIds, rewrapTxError)


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
        withSafeSigners sks (pure passphrase) $ \mss -> do
            ss <- maybeThrow (RequestError "Passphrase doesn't match") mss
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
