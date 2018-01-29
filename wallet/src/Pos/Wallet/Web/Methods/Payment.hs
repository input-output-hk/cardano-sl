{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( newPayment
       , newPaymentBatch
       , getTxFee
       ) where

import           Universum

import           Control.Exception.Safe (impureThrow)
import           Control.Monad.Except (runExcept)
import qualified Data.Map as M
import           Data.Time.Units (Second)
import           Mockable (Concurrently, Delay, Mockable, concurrently, delay)
import           Servant.Server (err405, errReasonPhrase)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses)
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.Network (prepareMTx)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), computeTxFee, runTxCreator)
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core (Coin, TxAux (..), TxOut (..), getCurrentTimestamp)
import           Pos.Core.Txp (_txOutputs)
import           Pos.Crypto (PassPhrase, ShouldCheckPassphrase (..), checkPassMatches, hash,
                             withSafeSignerUnsafe)
import           Pos.DB (MonadGState)
import           Pos.Txp (TxFee (..), Utxo)
import           Pos.Util (eitherToThrow, maybeThrow)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CCoin, CId, CTx (..),
                                             CWAddressMeta (..), NewBatchPayment (..), Wal,
                                             addrMetaToAccount)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Misc (convertCIdTOAddrs)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull, coinDistrToOutputs,
                                             getPendingAddresses, rewrapTxError,
                                             submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (Ever, Existing),
                                       MonadWalletDBRead)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                      getWalletAccountIds, getWalletAddrsDetector)

newPayment
    :: MonadWalletTxFull ctx m
    => PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPayment passphrase srcAccount dstAddress coin policy =
    -- This is done for two reasons:
    -- 1. In order not to overflow relay.
    -- 2. To let other things (e. g. block processing) happen if
    -- `newPayment`s are done continuously.
    notFasterThan (6 :: Second) $
      sendMoney
          passphrase
          (AccountMoneySource srcAccount)
          (one (dstAddress, coin))
          policy

newPaymentBatch
    :: MonadWalletTxFull ctx m
    => PassPhrase
    -> NewBatchPayment
    -> m CTx
newPaymentBatch passphrase NewBatchPayment {..} = do
    src <- decodeCTypeOrFail npbFrom
    notFasterThan (6 :: Second) $
      sendMoney
        passphrase
        (AccountMoneySource src)
        npbTo
        npbInputSelectionPolicy

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
     -> InputSelectionPolicy
     -> m CCoin
getTxFee srcAccount dstAccount coin policy = do
    pendingAddrs <- getPendingAddresses policy
    utxo <- getMoneySourceUtxo (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator policy (computeTxFee pendingAddrs utxo outputs)
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
    map adiCWAddressMeta <$> getAccountAddrsOrThrow Existing accId
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
    -> InputSelectionPolicy
    -> m CTx
sendMoney passphrase moneySource dstDistr policy = do
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }

    let srcWallet = getMoneySourceWallet moneySource
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")
    addrMetas' <- getMoneySourceAddresses moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    srcAddrs <- convertCIdTOAddrs $ map cwamId addrMetas

    logDebug "sendMoney: processed addrs"

    let metasAndAdrresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let getSigner addr = runIdentity $ do
          let addrMeta =
                  fromMaybe (error "Corresponding adress meta not found")
                            (M.lookup addr metasAndAdrresses)
          case runExcept $ getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta of
              Left err -> impureThrow err
              Right sk -> withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount moneySource
    outputs <- coinDistrToOutputs dstDistr
    pendingAddrs <- getPendingAddresses policy
    th <- rewrapTxError "Cannot send transaction" $ do
        (txAux, inpTxOuts') <-
            prepareMTx getSigner pendingAddrs policy srcAddrs outputs (relatedAccount, passphrase)

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
    _ <- addHistoryTxMeta srcWallet th
    diff <- getCurChainDifficulty
    srcWalletAddrsDetector <- getWalletAddrsDetector Ever srcWallet

    logDebug "sendMoney: constructing response"
    fst <$> constructCTx srcWallet srcWalletAddrsDetector diff th

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (Mockable Concurrently m, Mockable Delay m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
