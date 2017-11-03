{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( newPayment
       , getTxFee
       ) where

import           Universum

import           Control.Exception              (throw)
import           Control.Monad.Except           (runExcept)

import           Pos.Client.KeyStorage          (getSecretKeys)
import           Pos.Client.Txp.Addresses       (MonadAddresses)
import           Pos.Client.Txp.Balances        (MonadBalances (..))
import           Pos.Client.Txp.History         (TxHistoryEntry (..))
import           Pos.Client.Txp.Util            (computeTxFee, runTxCreator)
import           Pos.Communication              (prepareMTx)
import           Pos.Core                       (Coin, getCurrentTimestamp)
import           Pos.Crypto                     (PassPhrase, ShouldCheckPassphrase (..),
                                                 checkPassMatches, hash,
                                                 withSafeSignerUnsafe)
import           Pos.DB                         (MonadGState)
import           Pos.Txp                        (TxFee (..), Utxo, _txOutputs)
import           Pos.Txp.Core                   (TxAux (..), TxOut (..))
import           Pos.Util                       (eitherToThrow, maybeThrow)
import           Pos.Util.Servant               (encodeCType)
import           Pos.Wallet.Aeson.ClientTypes   ()
import           Pos.Wallet.Aeson.WalletBackup  ()
import           Pos.Wallet.Web.Account         (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes     (AccountId (..), Addr, CCoin, CId,
                                                 CTx (..), CWAddressMeta (..), Wal,
                                                 addrMetaToAccount)
import           Pos.Wallet.Web.Error           (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp     (MonadWalletTxFull, coinDistrToOutputs,
                                                 rewrapTxError, submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending         (mkPendingTx)
import           Pos.Wallet.Web.State           (AddressLookupMode (Ever, Existing),
                                                 MonadWalletDBRead)
import           Pos.Wallet.Web.Util            (decodeCTypeOrFail,
                                                 getAccountAddrsOrThrow,
                                                 getWalletAccountIds, getWalletAddrsSet)

newPayment
    :: MonadWalletTxFull ctx m
    => PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> m CTx
newPayment passphrase srcAccount dstAddress coin =
    sendMoney
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAddress, coin))

type MonadFees ctx m =
    ( MonadCatch m
    , MonadGState m
    , MonadWalletDBRead ctx m
    , MonadAddresses m
    , MonadBalances m
    )

getTxFee
     :: MonadFees ctx m
     => AccountId
     -> CId Addr
     -> Coin
     -> m CCoin
getTxFee srcAccount dstAccount coin = do
    utxo <- getMoneySourceUtxo (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator (computeTxFee utxo outputs)
    pure $ encodeCType fee

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource CWAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses
    :: (MonadThrow m, MonadWalletDBRead ctx m)
    => MoneySource -> m [CWAddressMeta]
getMoneySourceAddresses (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses (AccountMoneySource accId) =
    getAccountAddrsOrThrow Existing accId
getMoneySourceAddresses (WalletMoneySource wid) =
    getWalletAccountIds wid >>=
    concatMapM (getMoneySourceAddresses . AccountMoneySource)

getSomeMoneySourceAccount
    :: (MonadThrow m, MonadWalletDBRead ctx m)
    => MoneySource -> m AccountId
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

getMoneySourceUtxo
    :: (MonadThrow m, MonadWalletDBRead ctx m, MonadBalances m)
    => MoneySource -> m Utxo
getMoneySourceUtxo =
    getMoneySourceAddresses >=>
    mapM (decodeCTypeOrFail . cwamId) >=>
    getOwnUtxos

sendMoney
    :: (MonadWalletTxFull ctx m)
    => PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m CTx
sendMoney passphrase moneySource dstDistr = do
    let srcWallet = getMoneySourceWallet moneySource
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")
    addrMetas' <- getMoneySourceAddresses moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    srcAddrs <- forM addrMetas $ decodeCTypeOrFail . cwamId
    let metasAndAdrresses = zip (toList addrMetas) (toList srcAddrs)
    allSecrets <- getSecretKeys

    let getSinger addr = runIdentity $ do
          let addrMeta =
                  fromMaybe (error "Corresponding adress meta not found")
                            (fst <$> find ((== addr) . snd) metasAndAdrresses)
          case runExcept $ getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta of
              Left err -> throw err
              Right sk -> withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount moneySource
    outputs <- coinDistrToOutputs dstDistr
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts') <-
            prepareMTx getSinger srcAddrs outputs (relatedAccount, passphrase)

        ts <- Just <$> getCurrentTimestamp
        let tx = taTx txAux
            txHash = hash tx
            inpTxOuts = toList inpTxOuts'
            dstAddrs  = map txOutAddress . toList $
                        _txOutputs tx
            th = THEntry txHash tx Nothing inpTxOuts dstAddrs ts
        ptx <- mkPendingTx srcWallet txHash txAux th

        th <$ submitAndSaveNewPtx ptx

    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    addHistoryTxMeta srcWallet th
    srcWalletAddrs <- getWalletAddrsSet Ever srcWallet
    diff <- getCurChainDifficulty
    fst <$> constructCTx srcWallet srcWalletAddrs diff th
