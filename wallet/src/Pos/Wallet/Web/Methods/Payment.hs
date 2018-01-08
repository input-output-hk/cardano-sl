{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( newPayment
       , getTxFee
       ) where

import           Universum

import           Control.Exception                (throw)
import           Control.Monad.Except             (runExcept)
import qualified Data.Map                         as M
import           Data.Time.Units                  (Second)
import           Servant.Server                   (err405, errReasonPhrase)
import           Mockable                         (concurrently, delay)
import           System.Wlog                      (logDebug)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Aeson.WalletBackup           ()
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (getOwnUtxos)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Client.Txp.Util              (InputSelectionPolicy, computeTxFee,
                                                   runTxCreator)
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
import           Pos.Txp                          (TxFee (..), Utxo, _txOutputs)
import           Pos.Txp.Core                     (TxAux (..), TxOut (..))
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util                         (eitherToThrow, maybeThrow)
import           Pos.Wallet.KeyStorage            (getSecretKeys)
import           Pos.Wallet.Web.Account           (GenSeed (..), getSKByAddressPure,
                                                   getSKById)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CAddress (..),
                                                   CCoin, CId, CTx (..),
                                                   CWAddressMeta (..), Wal,
                                                   addrMetaToAccount, mkCCoin)
import           Pos.Wallet.Web.Error             (WalletError (..))
import           Pos.Wallet.Web.Methods.History   (addHistoryTx, constructCTx,
                                                   getCurChainDifficulty)
import qualified Pos.Wallet.Web.Methods.Logic     as L
import           Pos.Wallet.Web.Methods.Txp       (coinDistrToOutputs, rewrapTxError,
                                                   submitAndSaveNewPtx)
import           Pos.Wallet.Web.Mode              (MonadWalletWebMode, WalletWebMode,
                                                   convertCIdTOAddrs)
import           Pos.Wallet.Web.Pending           (mkPendingTx)
import           Pos.Wallet.Web.State             (AddressLookupMode (Ever, Existing))
import           Pos.Wallet.Web.Util              (decodeCTypeOrFail,
                                                   getAccountAddrsOrThrow,
                                                   getWalletAccountIds, getWalletAddrsSet)

newPayment
    :: MonadWalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPayment sa passphrase srcAccount dstAccount coin policy =
    notFasterThan (1 :: Second) $  -- in order not to overflow relay
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))
        policy
  where
    notFasterThan time action = fst <$> concurrently action (delay time)

getTxFee
     :: MonadWalletWebMode m
     => AccountId
     -> CId Addr
     -> Coin
     -> InputSelectionPolicy
     -> m CCoin
getTxFee srcAccount dstAccount coin policy = do
    utxo <- getMoneySourceUtxo (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator policy (computeTxFee utxo outputs)
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
    -> InputSelectionPolicy
    -> m CTx
sendMoney SendActions{..} passphrase moneySource dstDistr policy = do
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

    let getSinger addr = runIdentity $ do
          let addrMeta =
                  fromMaybe (error "Corresponding adress meta not found")
                            (M.lookup addr metasAndAdrresses)
          case runExcept $ getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta of
              Left err -> throw err
              Right sk -> withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount moneySource
    outputs <- coinDistrToOutputs dstDistr
    th <- rewrapTxError "Cannot send transaction" $ do
        logDebug "sendMoney: we're to prepareMTx"
        (txAux, inpTxOuts') <-
            prepareMTx getSinger policy srcAddrs outputs (relatedAccount, passphrase)
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
