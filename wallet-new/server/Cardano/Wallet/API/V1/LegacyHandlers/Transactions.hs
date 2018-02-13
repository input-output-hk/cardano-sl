{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.API.V1.LegacyHandlers.Transactions where

import           Universum

import           Pos.Core (TxAux)
import           Cardano.Wallet.API.V1.Migration (HasCompileInfo, HasConfigurations, MonadV1,
                                                  migrate)
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Client.Txp.Util as V0
import           Pos.Util (eitherToThrow)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types
import qualified Data.IxSet.Typed as IxSet

import           Data.Default
import qualified Data.List.NonEmpty as NE
import           Servant

handlers :: ( HasConfigurations
            , HasCompileInfo
            )
         => (TxAux -> MonadV1 Bool) -> ServerT Transactions.API MonadV1

handlers submitTx =
             newTransaction submitTx
        :<|> allTransactions
        :<|> estimateFees

newTransaction
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => (TxAux -> m Bool) -> Payment -> m (WalletResponse Transaction)
newTransaction submitTx Payment {..} = do
    let spendingPw = fromMaybe mempty pmtSpendingPassword
    cAccountId <- migrate (pmtSourceWallet, pmtSourceAccount)
    addrCoinList <- migrate $ NE.toList pmtDestinations
    policy <- migrate $ fromMaybe def pmtGroupingPolicy
    let batchPayment = V0.NewBatchPayment cAccountId addrCoinList policy
    cTx <- V0.newPaymentBatch submitTx spendingPw batchPayment
    single <$> migrate cTx

-- | The conclusion is that we want just the walletId for now, the details
-- in CSL-1917.
allTransactions
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => WalletId
    -> RequestParams
    -> m (WalletResponse [Transaction])
allTransactions walletId requestParams = do
    cIdWallet    <- migrate walletId

    -- TODO(ks): We need the type signature, fix this?
    let transactions :: m [Transaction]
        transactions = do
            (V0.WalletHistory wh, V0.WalletHistorySize whs) <-
                V0.getHistory cIdWallet mempty Nothing
            migrate (wh, whs)

    respondWith requestParams (NoFilters :: FilterOperations Transaction)
                              (NoSorts :: SortOperations Transaction)
                              (IxSet.fromList <$> transactions)

estimateFees :: (MonadThrow m, V0.MonadFees ctx m)
    => Payment
    -> m (WalletResponse EstimatedFees)
estimateFees Payment{..} = do
    policy <- migrate $ fromMaybe def pmtGroupingPolicy
    pendingAddrs <- V0.getPendingAddresses policy
    cAccountId <- migrate (pmtSourceWallet, pmtSourceAccount)
    utxo <- V0.getMoneySourceUtxo (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    fee <- V0.rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< V0.runTxCreator policy (V0.computeTxFee pendingAddrs utxo outputs)
    single <$> migrate fee
