module Cardano.Wallet.API.V1.LegacyHandlers.Transactions where

import           Universum

import qualified Data.IxSet.Typed as IxSet
import qualified Data.List.NonEmpty as NE
import           Servant

import           Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import qualified Pos.Client.Txp.Util as V0
import           Pos.Core (TxAux)
import qualified Pos.Core as Core
import qualified Pos.Util.Servant as V0
import qualified Pos.Wallet.WalletMode as V0
import qualified Pos.Wallet.Web.ClientTypes.Types as V0
import qualified Pos.Wallet.Web.Methods.History as V0
import qualified Pos.Wallet.Web.Methods.Payment as V0
import qualified Pos.Wallet.Web.Methods.Txp as V0
import qualified Pos.Wallet.Web.State as V0
import           Pos.Wallet.Web.State.Storage (WalletInfo (_wiSyncStatistics))
import qualified Pos.Wallet.Web.Util as V0

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Response
import           Cardano.Wallet.API.V1.Errors
import           Cardano.Wallet.API.V1.Migration (HasCompileInfo, HasConfigurations, MonadV1,
                                                  migrate)
import qualified Cardano.Wallet.API.V1.Transactions as Transactions
import           Cardano.Wallet.API.V1.Types

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
    single <$> migrate cTx

-- | By default, fees are separate from a posted payment. When you submit
-- a 'Payment' to the API, you are saying "Please send Alice 5 ADA, and the fees
-- will come out of my balance." The @feesIncluded@ parameter allows us to
-- include those fees in the payment, allowing us to express "Please send Bob
-- 3 ADA, and fees will come out of the ADA I am sending."
--
-- When fees are "included" in the payment amount, this means that we need to
-- somehow subtract the fee amount computed for the 'Transaction' from the
-- outputs in the 'Payment''s 'PaymentDistribution's. For the trivial case (a
-- single payment), this is easy -- we deduct the entire fee amount from the
-- transaction.
--
-- For payments with multiple outputs, we must consider carefully how to address
-- the fee. There are a number of possible ways to distribute the fee among
-- several 'PaymentDistribution's:
--
-- 1. Allow the user to specify the precise amount to deduct from each payment.
--    This would provide a somewhat complicated API for questionable gain.
-- 2. Deduct a proportion of the fee evenly from each payment. In pseudocode,
--    this is:
--
--    @
--    newOutputs fee outputs =
--        map (subtract (fee `div` length outputs)) outputs
--    @
--
--    This runs the risk of draining a 'PaymentDistribution's output to @0@,
--    which would likely be an error case (there's no reason to ever have an
--    output of @0@, unless you just really like paying fees for some reason).
--    For this reason, this mode is unsupported.
-- 3. Deduct an amount proportional to each payment. In pseudocode, this is:
--
--    @
--    newOutputs fee outputs =
--        let
--            total =
--                sum outputs
--            subtractProportion output =
--                output - (fee * (output / total))
--        in
--            map subtractProportion outputs
--    @
--
--    This makes the likelihood of zeroing out a payment much less likely -- the
--    only way we can observe that now is in the strange case of tiny payments
--    where rounding causes it to zero out. We must still account for this, but
--    it is less likely.
--
-- The astute observer will note that we operate on 'Coin', a wrapper around
-- 'Word64' representing the number of Lovelace coins, but the operations
-- require rational numbers. This implies the potential for rounding issues.
-- Rounding issues give us three options: 'round', 'floor', and 'ceiling'.
--
-- Let's consider the strategies with the following data:
--
-- >>> outputs = [5, 8, 10]
-- >>> total = sum outputs
-- >>> fee = 3
-- >>> withProportions xs = map (\x -> (x, fromIntegral x / fromIntegral (sum xs)))
-- >>> withProportions outputs
-- [(5,0.21739130434782608),(8,0.34782608695652173),(10,0.43478260869565216)]
-- >>> subtractProportion output = fromIntegral output - (fee * (output / total))
-- >>> let precise = map (\o -> fromIntegral o - (fee * (o / total))) outputs
-- >>> precise
-- [4.875,7.8,9.75]
--
-- Our initial total spending was 23, and we want to deduct a fee of 3 from this
-- amount, for a total disbursement of 20 to these outputs. Let's observe the
-- result of the different options:
--
-- >>> sum (map round precise)
-- 23
--
-- Well, 'round' appears to be precisely what we want! The total lines up
-- exactly. Is this by chance? We can write a QuickCheck property to test it.
newTransactionFeesIncluded
    :: forall ctx m . (V0.MonadWalletTxFull ctx m)
    => (TxAux -> m Bool) -> Payment -> m (WalletResponse Transaction)
newTransactionFeesIncluded submitTx pmt = do
    EstimatedFees (V1 coin) <- wrData <$> estimateFees pmt
    let outputs = length (pmtDestinations pmt)
        coinPerOutput = Core.getCoin coin `div` fromIntegral outputs
        newPayment = pmt
            { pmtDestinations =
                map subtractAmount (pmtDestinations pmt)
            }
        subtractAmount pmtDistribution = pmtDistribution
            { pdAmount = V1 $
                (unV1 (pdAmount pmtDistribution))
                    `Core.unsafeSubCoin`
                        Core.mkCoin coinPerOutput
            }

    newTransaction submitTx newPayment

-- | This function implements the fee distribution logic described in the
-- comments of 'newTransactionFeesIncluded'. There are two preconditions, and
-- this function returns 'Nothing' if either are violated:
--
-- 1. The fee amount must be less than the total of the outputs. If this is
--    violated, then it would be impossible for the fee to come entirely from
--    the outputs, so the transaction is invalid.
-- 2. None of the outputs may be 0.
distributeFeesInternal
    :: Word64 -- ^ The fee amount
    -> NonEmpty Word64 -- ^ The outputs of the 'PaymentDistribution'
    -> Maybe (NonEmpty Rational) -- ^ The resulting outputs after subtracting the fee.
distributeFeesInternal fee outputs
    | fromIntegral fee >= total || any (0 ==) outputs = Nothing
    | otherwise = Just result
  where
    result =
        map (subtractFee) outputs
    total =
        sum (map fromIntegral outputs)
    subtractFee output =
        fromIntegral output - (fromIntegral fee * (fromIntegral output / total))


-- | This subtraction implementation bottoms out at @0@ instead of underflowing.
subBottomOut :: Word64 -> Word64 -> Word64
subBottomOut x y
    | result > x = 0
    | otherwise = result
  where
    result = x - y

allTransactions
    :: forall ctx m. (V0.MonadWalletHistory ctx m)
    => Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe (V1 Core.Address)
    -> RequestParams
    -> FilterOperations Transaction
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

            -- Paginate result
            respondWith requestParams fops sops (IxSet.fromList <$> transactions)
        _ ->
            -- TODO: should we use the 'FilterBy' machinery instead? that
            --       let us express RANGE, GT, etc. in addition to EQ. does
            --       that make sense for this dataset?
            throwM MissingRequiredParams
                { requiredParams = pure ("wallet_id", "WalletId")
                }

estimateFees :: (MonadThrow m, V0.MonadFees ctx m)
    => Payment
    -> m (WalletResponse EstimatedFees)
estimateFees Payment{..} = do
    ws <- V0.askWalletSnapshot
    let (V1 policy) = fromMaybe (V1 defaultInputSelectionPolicy) pmtGroupingPolicy
        pendingAddrs = V0.getPendingAddresses ws policy
    cAccountId <- migrate pmtSource
    utxo <- V0.getMoneySourceUtxo ws (V0.AccountMoneySource cAccountId)
    outputs <- V0.coinDistrToOutputs =<< mapM migrate pmtDestinations
    efee <- V0.runTxCreator policy (V0.computeTxFee pendingAddrs utxo outputs)
    case efee of
        Right fee ->
            single <$> migrate fee
        Left err ->
            throwM (convertTxError err)
