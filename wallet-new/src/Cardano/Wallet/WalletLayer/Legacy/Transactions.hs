module Cardano.Wallet.WalletLayer.Legacy.Transactions
    ( pwlCreateTx
    , pwlGetTxs
    , pwlEstimateFees
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration (migrate)
import           Cardano.Wallet.API.V1.Types
import qualified Data.List.NonEmpty as NE
import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import qualified Pos.Client.Txp.Util as V0
import           Pos.Core (TxAux)
import qualified Pos.Core as Core
import           Pos.Util (eitherToThrow)
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Wallet.Web.State as V0
import           Pos.Wallet.Web.State.Storage (WalletInfo (_wiSyncStatistics))
import qualified Pos.Wallet.Web.Util as V0


pwlCreateTx
    :: forall ctx m. (V0.MonadWalletTxFull ctx m)
    => (TxAux -> m Bool)
    -> Payment
    -> m Transaction
pwlCreateTx submitTx Payment {..} = do
    ws <- V0.askWalletSnapshot
    sourceWallet <- migrate (psWalletId pmtSource)

    -- If the wallet is being restored, we need to disallow any @Payment@ from
    -- being submitted.
    -- FIXME(adn): make grabbing a 'V1.SyncState' from the old data layer
    -- easier and less verbose.
    when (V0.isWalletRestoring ws sourceWallet) $ do
        let stats    = _wiSyncStatistics <$> V0.getWalletInfo ws sourceWallet
        currentHeight <- V0.networkChainDifficulty
        progress <- case liftM2 (,) stats currentHeight  of
                        Nothing     -> pure $ SyncProgress (mkEstimatedCompletionTime 0)
                                                           (mkSyncThroughput (Core.BlockCount 0))
                                                           (mkSyncPercentage 0)
                        Just (s, h) -> migrate (s, Just h)
        throwM $ WalletIsNotReadyToProcessPayments progress

    let (V1 spendingPw) = fromMaybe (V1 mempty) pmtSpendingPassword
    cAccountId <- migrate pmtSource
    addrCoinList <- migrate $ NE.toList pmtDestinations
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
    let batchPayment = V0.NewBatchPayment cAccountId addrCoinList policy
    cTx <- V0.newPaymentBatch submitTx spendingPw batchPayment
    migrate cTx


pwlGetTxs
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> m [Transaction]
pwlGetTxs mwalletId mAccIdx mAddr =
    case mwalletId of
        Just walletId -> do
            cIdWallet <- migrate walletId
            ws <- V0.askWalletSnapshot

            -- Create a `[V0.AccountId]` to get txs from it
            let accIds = case mAccIdx of
                    Just accIdx -> migrate (walletId, accIdx)
                    -- ^ Migrate `V1.AccountId` into `V0.AccountId` and put it into a list
                    Nothing     -> V0.getWalletAccountIds ws cIdWallet
                    -- ^ Or get all `V0.AccountId`s of a wallet

            let v0Addr = case mAddr of
                    Nothing        -> Nothing
                    Just (V1 addr) -> Just $ V0.encodeCType addr

            -- get all `[Transaction]`'s
            let transactions = do
                    (V0.WalletHistory wh, _) <- V0.getHistory cIdWallet (const accIds) v0Addr
                    migrate wh

            transactions

        _ ->
            -- TODO: should we use the 'FilterBy' machinery instead? that
            --       let us express RANGE, GT, etc. in addition to EQ. does
            --       that make sense for this dataset?
            throwM MissingRequiredParams
                { requiredParams = pure ("wallet_id", "WalletId")
                }


pwlEstimateFees
    :: forall ctx m. (MonadThrow m, V0.MonadFees ctx m)
    => Payment
    -> m EstimatedFees
pwlEstimateFees Payment{..} = do
    ws <- V0.askWalletSnapshot
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
        pendingAddrs = V0.getPendingAddresses ws policy
    cAccountId <- migrate pmtSource
    utxo <- V0.getMoneySourceUtxo ws (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    fee <- V0.rewrapTxError "Cannot compute transaction fee" $
        eitherToThrow =<< V0.runTxCreator policy (V0.computeTxFee pendingAddrs utxo outputs)
    migrate fee
