{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transaction creation and fees

module Pos.Wallet.Web.Methods.Payment
       ( MoneySource(..)
       , MonadFees
       , getMoneySourceUtxo
       , newPayment
       , newPaymentBatch
       , getTxFee
       ) where

import           Universum

import           Control.Monad.Except (runExcept)
import qualified Data.Map as M
import           Data.Time.Units (Second)
import           Mockable (Concurrently, Delay, Mockable, concurrently, delay)
import           Servant.Server (err403, err405, errReasonPhrase)
import           System.Wlog (logDebug)

import           Pos.Client.KeyStorage (getSecretKeys)
import           Pos.Client.Txp.Addresses (MonadAddresses)
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Client.Txp.Network (prepareMTx)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..), computeTxFee, runTxCreator)
import           Pos.Configuration (walletTxCreationDisabled)
import           Pos.Core (Address, Coin, HasConfiguration, TxAux (..), TxOut (..),
                           getCurrentTimestamp)
import           Pos.Core.Txp (_txOutputs)
import           Pos.Crypto (PassPhrase, SafeSigner, ShouldCheckPassphrase (..), checkPassMatches,
                             hash, withSafeSignerUnsafe)
import           Pos.DB (MonadGState)
import           Pos.Txp (TxFee (..), Utxo)
import           Pos.Util (eitherToThrow, maybeThrow)
import           Pos.Util.Servant (encodeCType)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.WalletBackup ()
import           Pos.Wallet.Web.Account (getSKByAddressPure, getSKById)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CCoin, CId, CTx (..),
                                             NewBatchPayment (..), Wal)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta, constructCTx,
                                                 getCurChainDifficulty)
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull, coinDistrToOutputs,
                                             getPendingAddresses, rewrapTxError,
                                             submitAndSaveNewPtx)
import           Pos.Wallet.Web.Pending (mkPendingTx)
import           Pos.Wallet.Web.State (AddressInfo (..), AddressLookupMode (Ever, Existing),
                                       HasWAddressMeta (..), WAddressMeta (..), WalletDbReader,
                                       WalletSnapshot, askWalletDB, askWalletSnapshot,
                                       getWalletSnapshot, isWalletRestoring, wamAccount)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail, getAccountAddrsOrThrow,
                                      getWalletAccountIds, getWalletAddrsDetector)

newPayment
    :: MonadWalletTxFull ctx m
    => (TxAux -> m Bool)
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> InputSelectionPolicy
    -> m CTx
newPayment submitTx passphrase srcAccount dstAddress coin policy =
    -- This is done for two reasons:
    -- 1. In order not to overflow relay.
    -- 2. To let other things (e. g. block processing) happen if
    -- `newPayment`s are done continuously.
    notFasterThan (6 :: Second) $ do
      sendMoney
          submitTx
          passphrase
          (AccountMoneySource srcAccount)
          (one (dstAddress, coin))
          policy

newPaymentBatch
    :: MonadWalletTxFull ctx m
    => (TxAux -> m Bool)
    -> PassPhrase
    -> NewBatchPayment
    -> m CTx
newPaymentBatch submitTx passphrase NewBatchPayment {..} = do
    src <- decodeCTypeOrFail npbFrom
    notFasterThan (6 :: Second) $ do
      sendMoney
          submitTx
          passphrase
          (AccountMoneySource src)
          npbTo
          npbInputSelectionPolicy

type MonadFees ctx m =
    ( MonadCatch m
    , MonadGState m
    , WalletDbReader ctx m
    , MonadAddresses m
    , MonadBalances m
    , MonadIO m
    , HasConfiguration
    )

getTxFee
     :: MonadFees ctx m
     => AccountId
     -> CId Addr
     -> Coin
     -> InputSelectionPolicy
     -> m CCoin
getTxFee srcAccount dstAccount coin policy = do
    ws <- askWalletSnapshot
    let pendingAddrs = getPendingAddresses ws policy
    utxo <- getMoneySourceUtxo ws (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< runTxCreator policy (computeTxFee pendingAddrs utxo outputs)
    pure $ encodeCType fee

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource WAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: MonadThrow m
                        => WalletSnapshot
                        -> MoneySource
                        -> m [WAddressMeta]
getMoneySourceAddresses _ (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses ws (AccountMoneySource accId) =
    map adiWAddressMeta <$> getAccountAddrsOrThrow ws Existing accId
getMoneySourceAddresses ws (WalletMoneySource wid) =
    concatMapM (getMoneySourceAddresses ws . AccountMoneySource)
               (getWalletAccountIds ws wid)

getSomeMoneySourceAccount :: MonadThrow m
                          => WalletSnapshot -> MoneySource -> m AccountId
getSomeMoneySourceAccount _ (AddressMoneySource addrId) =
    return $ addrId ^. wamAccount
getSomeMoneySourceAccount _ (AccountMoneySource accId) = return accId
getSomeMoneySourceAccount ws (WalletMoneySource wid) = do
    wAddr <- maybeThrow noWallets (head (getWalletAccountIds ws wid))
    getSomeMoneySourceAccount ws (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

getMoneySourceWallet :: MoneySource -> CId Wal
getMoneySourceWallet (AddressMoneySource addrId) = addrId ^. wamWalletId
getMoneySourceWallet (AccountMoneySource accId)  = aiWId accId
getMoneySourceWallet (WalletMoneySource wid)     = wid

getMoneySourceUtxo :: (MonadThrow m, MonadBalances m)
                   => WalletSnapshot
                   -> MoneySource
                   -> m Utxo
getMoneySourceUtxo ws =
    getMoneySourceAddresses ws >=>
    mapM (return . view wamAddress) >=>
    getOwnUtxos

sendMoney
    :: (MonadWalletTxFull ctx m)
    => (TxAux -> m Bool)
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> InputSelectionPolicy
    -> m CTx
sendMoney submitTx passphrase moneySource dstDistr policy = do
    db <- askWalletDB
    ws <- getWalletSnapshot db
    when walletTxCreationDisabled $
        throwM err405
        { errReasonPhrase = "Transaction creation is disabled by configuration!"
        }
    let srcWallet = getMoneySourceWallet moneySource
    when (isWalletRestoring ws srcWallet) $
        throwM err403
        { errReasonPhrase = "Transaction creation is disabled when the wallet is restoring."
        }
    rootSk <- getSKById srcWallet
    checkPassMatches passphrase rootSk `whenNothing`
        throwM (RequestError "Passphrase doesn't match")

    addrMetas' <- getMoneySourceAddresses ws moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")

    let srcAddrs = map (view wamAddress) addrMetas

    logDebug "sendMoney: processed addrs"

    let metasAndAddresses = M.fromList $ zip (toList srcAddrs) (toList addrMetas)
    allSecrets <- getSecretKeys

    let
        getSigner :: Address -> Maybe SafeSigner
        getSigner addr = do
          addrMeta <- M.lookup addr metasAndAddresses
          sk <- rightToMaybe . runExcept $
              getSKByAddressPure allSecrets (ShouldCheckPassphrase False) passphrase addrMeta
          withSafeSignerUnsafe sk (pure passphrase) pure

    relatedAccount <- getSomeMoneySourceAccount ws moneySource
    outputs <- coinDistrToOutputs dstDistr
    let pendingAddrs = getPendingAddresses ws policy
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
        ptx <- mkPendingTx ws srcWallet txHash txAux th

        th <$ submitAndSaveNewPtx db submitTx ptx

    -- We add TxHistoryEntry's meta created by us in advance
    -- to make TxHistoryEntry in CTx consistent with entry in history.
    _ <- addHistoryTxMeta db srcWallet th
    diff <- getCurChainDifficulty
    ws' <- getWalletSnapshot db
    let srcWalletAddrsDetector = getWalletAddrsDetector ws' Ever srcWallet

    logDebug "sendMoney: constructing response"
    fst <$> constructCTx ws' srcWallet srcWalletAddrsDetector diff th

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (Mockable Concurrently m, Mockable Delay m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
