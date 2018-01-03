{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( ReformCanceledTxsParams (..)
       , newPayment
       , getTxFee
       , reformCanceledTxs
       ) where

import           Universum

import           Control.Exception                (throw)
import           Control.Lens                     (ix)
import           Control.Monad.Catch              (handleAll)
import           Control.Monad.Except             (runExcept)
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Data.Time.Units                  (Second)
import           Formatting                       (build, sformat, shown, (%))
import           Mockable                         (concurrently, delay)
import           Serokell.Util.Text               (listJson)
import           Servant.Server                   (err405, errReasonPhrase)
import           System.Wlog                      (logDebug, logInfo, logWarning)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Aeson.WalletBackup           ()
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (getOwnUtxos)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Client.Txp.Util              (computeTxFee, runTxCreator)
import           Pos.Communication                (SendActions (..), prepareMTx)
import           Pos.Configuration                (HasNodeConfiguration,
                                                   walletTxCreationDisabled)
import           Pos.Core                         (Coin, HasConfiguration,
                                                   getCurrentTimestamp)
import           Pos.Crypto                       (PassPhrase, ShouldCheckPassphrase (..),
                                                   checkPassMatches, hash,
                                                   withSafeSignerUnsafe)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (TxFee (..), TxId, Utxo, _txOutputs)
import           Pos.Txp.Core                     (TxAux (..), TxOut (..))
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util                         (eitherToThrow, maybeThrow)
import           Pos.Util.Servant                 (encodeCType)
import           Pos.Wallet.KeyStorage            (getSecretKeys)
import           Pos.Wallet.Web.Account           (GenSeed (..), getSKByAddressPure,
                                                   getSKById)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CAddress (..),
                                                   CCoin, CId, CTx (..),
                                                   CWAddressMeta (..),
                                                   ReformCanceledTxsParams (..), Wal,
                                                   addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error             (WalletError (..))
import           Pos.Wallet.Web.Methods.History   (addHistoryTx, constructCTx,
                                                   getCurChainDifficulty)
import qualified Pos.Wallet.Web.Methods.Logic     as L
import           Pos.Wallet.Web.Methods.Txp       (coinDistrToOutputs, rewrapTxError,
                                                   submitAndSaveNewPtx)
import           Pos.Wallet.Web.Mode              (MonadWalletWebMode, WalletWebMode,
                                                   convertCIdTOAddrs)
import           Pos.Wallet.Web.Pending           (PendingTx (..), PtxCondition (..),
                                                   PtxPoolInfo, mkPendingTx)
import           Pos.Wallet.Web.State             (AddressLookupMode (Ever, Existing))
import           Pos.Wallet.Web.State.State       (cancelSpecificApplyingPtx,
                                                   getPendingTxs)
import           Pos.Wallet.Web.Util              (decodeCTypeOrFail,
                                                   getAccountAddrsOrThrow,
                                                   getWalletAccountIds, getWalletAddrsSet,
                                                   testOnlyEndpoint)

newPayment
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> m CTx
newPayment sa passphrase srcAccount dstAccount coin =
    notFasterThan (1 :: Second) $  -- in order not to overflow relay
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))
  where
    notFasterThan time action = fst <$> concurrently action (delay time)

getTxFee
     :: MonadWalletWebMode m
     => AccountId
     -> CId Addr
     -> Coin
     -> m CCoin
getTxFee srcAccount dstAccount coin = do
    pendingTxs <- getPendingTxs
    utxo <- getMoneySourceUtxo (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator (computeTxFee pendingTxs utxo outputs)
    pure $ mkCCoin fee

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource CWAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: MonadWalletWebMode m => MoneySource -> m [CWAddressMeta]
getMoneySourceAddresses (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses (AccountMoneySource accId) =
    getAccountAddrsOrThrow Existing accId
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

getMoneySourceUtxo :: MonadWalletWebMode m => MoneySource -> m Utxo
getMoneySourceUtxo =
    getMoneySourceAddresses >=>
    mapM (decodeCTypeOrFail . cwamId) >=>
    getOwnUtxos

-- [CSM-407] It should be moved to `Pos.Wallet.Web.Mode`, but
-- to make it possible all this mess should be neatly separated
-- to modules and refactored
instance
    ( HasConfiguration
    , HasNodeConfiguration
    , HasInfraConfiguration
    , HasGtConfiguration
    , HasUpdateConfiguration
    )
    => MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode
  where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    getNewAddress (accId, passphrase) = do
        clientAddress <- L.newAddress RandomSeed passphrase accId
        decodeCTypeOrFail (cadId clientAddress)

sendMoney
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m CTx
sendMoney SendActions{..} passphrase moneySource dstDistr = do
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }

    let srcWallet = getMoneySourceWallet moneySource
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")

    logDebug "sendMoney: start retrieving addrs"

    addrMetas' <- getMoneySourceAddresses moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")
    logDebug "sendMoney: retrieved addrs"

    srcAddrs <- convertCIdTOAddrs $ map cwamId addrMetas

    logDebug "sendMoney: processed addrs"

    let metasAndAdrresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let getSigner addr = runIdentity $ do
          let addrMeta =
                  fromMaybe (error "Corresponding adress meta not found")
                            (M.lookup addr metasAndAdrresses)
          case runExcept $ getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta of
              Left err -> throw err
              Right sk -> withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount moneySource
    outputs <- coinDistrToOutputs dstDistr
    pendingTxs <- getPendingTxs
    th <- rewrapTxError "Cannot send transaction" $ do
        logDebug "sendMoney: we're to prepareMTx"
        (txAux, inpTxOuts') <-
            prepareMTx pendingTxs getSigner srcAddrs outputs (relatedAccount, passphrase)
        logDebug "sendMoney: performed prepareMTx"

        ts <- Just <$> getCurrentTimestamp
        let tx = taTx txAux
            txHash = hash tx
            inpTxOuts = toList inpTxOuts'
            dstAddrs  = map txOutAddress . toList $
                        _txOutputs tx
            th = THEntry txHash tx Nothing inpTxOuts dstAddrs ts
        ptx <- mkPendingTx srcWallet txHash txAux th

        logDebug "sendMoney: performed mkPendingTx"
        submitAndSaveNewPtx enqueueMsg ptx
        logDebug "sendMoney: submitted and saved tx"

        return th

    addHistoryTx srcWallet th
    srcWalletAddrs <- getWalletAddrsSet Ever srcWallet
    diff <- getCurChainDifficulty

    logDebug "sendMoney: constructing response"
    fst <$> constructCTx srcWallet srcWalletAddrs diff th


-- | For each specified transaction, if it has been canceled, creates
-- new transaction with correspondent destination address and money.
-- This works as `mapM newPayment`, so completion may take a lot of time.
--
-- NOTE: Work of this function relies on how wallet create transactions
-- currently!
reformCanceledTxs
    :: MonadWalletWebMode m
    => SendActions m -> PassPhrase -> ReformCanceledTxsParams -> m [CTx]
reformCanceledTxs sendActions passphrase params = testOnlyEndpoint $ do
    ptxs <- getPendingTxs
    let canceledTxs :: [(TxId, (CId Wal, PtxPoolInfo))]
        canceledTxs = flip mapMaybe ptxs $ \PendingTx{..} ->
            case _ptxCond of
                PtxWontApply _ poolInfo -> Just (_ptxTxId, (_ptxWallet, poolInfo))
                _                       -> Nothing
    logDebug $ sformat ("List of all canceled transactions ever: "%listJson)
         (map fst canceledTxs)

    txsToRecreate <- case rctpBanned params of
        [] -> pure canceledTxs
        bannedCTxs -> do
            bannedTxs <- mapM decodeCTypeOrFail bannedCTxs
            let bannedTxsSet = S.fromList bannedTxs
            pure $ filter ((`S.notMember` bannedTxsSet) . fst) canceledTxs

    logDebug $ sformat ("Transactions selected for recreation: "%listJson)
        (map fst txsToRecreate)

    fmap catMaybes . forM txsToRecreate $ \(txId, (wid, txEntry)) ->
        handleAll (txCreationHandler txId) $ do
            let moneySource = WalletMoneySource wid
                outputs = _txOutputs (_thTx txEntry)
            TxOut addr coin <-
                maybeThrow noExpectedInput (outputs ^? ix primaryDestIndex)
            let outDistr = (encodeCType addr, coin)
            ctx <- sendMoney sendActions passphrase moneySource (one outDistr)

            -- it's name is now misleading, it actually totally removes transaction
            -- disregard its status
            cancelSpecificApplyingPtx txId
            newTxId <- decodeCTypeOrFail (ctId ctx)
            logInfo $ sformat ("Successfully recreated transaction "%build
                              %", new transaction: "%build%". Old transaction \
                              \is removed")
                txId newTxId
            return (Just ctx)
  where
    -- for all current transactions we have change address as 0-th output
    -- and primary destination as 1-st output
    primaryDestIndex = 1
    noExpectedInput = InternalError "Transaction has no inputs"
    txCreationHandler txId e = do
        logWarning $ sformat ("Failed to recreate transaction "%build%": "%shown)
             txId e
        return Nothing
