{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.API.V1.LegacyHandlers.Transactions where

import           Universum

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration (HasCompileInfo, HasConfigurations, MonadV1,
                                                  migrate)
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types
import qualified Data.IxSet.Typed as IxSet
import qualified Data.List.NonEmpty as NE
import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import qualified Pos.Client.Txp.Util as V0
import           Pos.Core (TxAux)
import qualified Pos.Core as Core
import           Pos.Util (eitherToThrow)
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Wallet.Web.Util as V0
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
    let (V1 spendingPw) = fromMaybe (V1 mempty) pmtSpendingPassword
    cAccountId <- migrate pmtSource
    addrCoinList <- migrate $ NE.toList pmtDestinations
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
    let batchPayment = V0.NewBatchPayment cAccountId addrCoinList policy
    cTx <- V0.newPaymentBatch submitTx spendingPw batchPayment
    single <$> migrate cTx


allTransactions
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> RequestParams
    -> FilterOperations Transaction
    -> SortOperations Transaction
    -> m (WalletResponse [Transaction])
allTransactions walletId mAccIdx mAddr requestParams fops sops = do
    cIdWallet <- migrate walletId

    -- Create a `[V0.AccountId]` to get txs from it
    accIds <- case mAccIdx of
        Just accIdx -> pure $ migrate (walletId, accIdx)
        -- ^ Migrate `V1.AccountId` into `V0.AccountId` and put it into a list
        Nothing     -> V0.getWalletAccountIds cIdWallet
        -- ^ Or get all `V0.AccountId`s of a wallet

    let v0Addr = case mAddr of
            Nothing        -> Nothing
            Just (V1 addr) -> Just $ V0.encodeCType addr

    -- get all `[Transaction]`'s
    let transactions = do
            (V0.WalletHistory wh, _) <- V0.getHistory cIdWallet accIds v0Addr
            migrate wh

    -- Paginate result
    respondWith requestParams fops sops (IxSet.fromList <$> transactions)


estimateFees :: (MonadThrow m, V0.MonadFees ctx m)
    => Payment
    -> m (WalletResponse EstimatedFees)
estimateFees Payment{..} = do
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
    pendingAddrs <- V0.getPendingAddresses policy
    cAccountId <- migrate pmtSource
    utxo <- V0.getMoneySourceUtxo (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    fee <- V0.rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< V0.runTxCreator policy (V0.computeTxFee pendingAddrs utxo outputs)
    single <$> migrate fee
