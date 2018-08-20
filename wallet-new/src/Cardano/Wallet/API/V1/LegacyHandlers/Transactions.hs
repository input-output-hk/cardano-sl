module Cardano.Wallet.API.V1.LegacyHandlers.Transactions where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat)
import           Servant

import           Pos.Chain.Txp (TxId, TxpConfiguration)
import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import qualified Pos.Client.Txp.Util as V0
import qualified Pos.Core as Core
import           Pos.Core.Txp (TxAux)
import           Pos.Crypto (ProtocolMagic)
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Redeem as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Wallet.Web.State as V0
import           Pos.Wallet.Web.State.Storage (WalletInfo (_wiSyncStatistics))
import qualified Pos.Wallet.Web.Util as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Migration (HasConfigurations, MonadV1,
                     migrate)
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet


convertTxError :: V0.TxError -> WalletError
convertTxError err = case err of
    V0.NotEnoughMoney coin ->
        NotEnoughMoney . fromIntegral . Core.getCoin $ coin
    V0.NotEnoughAllowedMoney coin ->
        NotEnoughMoney . fromIntegral . Core.getCoin $ coin
    V0.FailedToStabilize ->
        TxFailedToStabilize
    V0.OutputIsRedeem addr ->
        OutputIsRedeem (V1 addr)
    V0.RedemptionDepleted ->
        TxRedemptionDepleted
    V0.SafeSignerNotFound addr ->
        TxSafeSignerNotFound (V1 addr)
    V0.SignedTxNotBase16Format ->
        SignedTxSubmitError $ sformat build V0.SignedTxNotBase16Format
    V0.SignedTxUnableToDecode txt ->
        SignedTxSubmitError $ sformat build (V0.SignedTxUnableToDecode txt)
    V0.SignedTxSignatureNotBase16Format ->
        SignedTxSubmitError $ sformat build V0.SignedTxSignatureNotBase16Format
    V0.SignedTxInvalidSignature txt ->
        SignedTxSubmitError $ sformat build (V0.SignedTxInvalidSignature txt)
    V0.GeneralTxError txt ->
        UnknownError txt


handlers
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> (TxAux -> MonadV1 Bool)
    -> ServerT Transactions.API MonadV1
handlers pm txpConfig submitTx =
             newTransaction pm txpConfig submitTx
        :<|> allTransactions
        :<|> estimateFees pm
        :<|> redeemAda pm txpConfig submitTx

newTransaction
    :: forall ctx m
     . (V0.MonadWalletTxFull ctx m)
    => ProtocolMagic
    -> TxpConfiguration
    -> (TxAux -> m Bool)
    -> Payment
    -> m (WalletResponse Transaction)
newTransaction pm txpConfig submitTx Payment {..} = do
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
    cTx <- V0.newPaymentBatch pm txpConfig submitTx spendingPw batchPayment
    single <$> migrate cTx


allTransactions
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> RequestParams
    -> FilterOperations '[V1 TxId, V1 Core.Timestamp] Transaction
    -> SortOperations Transaction
    -> m (WalletResponse [Transaction])
allTransactions mwalletId mAccIdx mAddr requestParams fops sops  =
    case mwalletId of
        Just walletId -> do
            cIdWallet <- migrate walletId
            ws <- V0.askWalletSnapshot

            -- Create a `[V0.AccountId]` to get txs from it
            let accIds = case mAccIdx of
                    Just accIdx -> migrate (walletId, accIdx)
                    -- Migrate `V1.AccountId` into `V0.AccountId` and put it into a list
                    Nothing     -> V0.getWalletAccountIds ws cIdWallet
                    -- Or get all `V0.AccountId`s of a wallet

            let v0Addr = case mAddr of
                    Nothing        -> Nothing
                    Just (V1 addr) -> Just $ V0.encodeCType addr

            -- get all `[Transaction]`'s
            let transactions = do
                    (V0.WalletHistory wh, _) <- V0.getHistory cIdWallet (const accIds) v0Addr
                    migrate wh

            -- Paginate result
            respondWith requestParams fops sops (IxSet.fromList <$> transactions)
        _ ->
            -- TODO: should we use the 'FilterBy' machinery instead? that
            --       let us express RANGE, GT, etc. in addition to EQ. does
            --       that make sense for this dataset?
            throwM . MissingRequiredParams $ pure ("wallet_id", "WalletId")

estimateFees
    :: (MonadThrow m, V0.MonadFees ctx m)
    => ProtocolMagic
    -> Payment
    -> m (WalletResponse EstimatedFees)
estimateFees pm Payment{..} = do
    ws <- V0.askWalletSnapshot
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
        pendingAddrs = V0.getPendingAddresses ws policy
    cAccountId <- migrate pmtSource
    utxo <- V0.getMoneySourceUtxo ws (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    efee <- V0.runTxCreator policy (V0.computeTxFee pm pendingAddrs utxo outputs)
    case efee of
        Right fee ->
            single <$> migrate fee
        Left err ->
            throwM (convertTxError err)

redeemAda
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> (TxAux -> MonadV1 Bool)
    -> Redemption
    -> MonadV1 (WalletResponse Transaction)
redeemAda pm txpConfig submitTx r = do
    let ShieldedRedemptionCode seed = redemptionRedemptionCode r
        V1 spendingPassword = redemptionSpendingPassword r
        walletId = redemptionWalletId r
        accountIndex = redemptionAccountIndex r
    accountId <- migrate (walletId, accountIndex)
    let caccountId = V0.encodeCType accountId
    fmap single . migrate =<< case redemptionMnemonic r of
        Just (RedemptionMnemonic mnemonic) -> do
            let phrase = V0.CBackupPhrase mnemonic
            let cpaperRedeem = V0.CPaperVendWalletRedeem
                    { V0.pvWalletId = caccountId
                    , V0.pvSeed = seed
                    , V0.pvBackupPhrase = phrase
                    }
            V0.redeemAdaPaperVend pm txpConfig submitTx spendingPassword cpaperRedeem
        Nothing -> do
            let cwalletRedeem = V0.CWalletRedeem
                    { V0.crWalletId = caccountId
                    , V0.crSeed = seed
                    }
            V0.redeemAda pm txpConfig submitTx spendingPassword cwalletRedeem
