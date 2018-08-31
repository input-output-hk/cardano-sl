module Cardano.Wallet.WalletLayer.Kernel.Active (
    pay
  , estimateFees
  , redeemAda
  ) where

import           Universum

import           Data.Time.Units (Second)

import           Pos.Core (Address, Coin, TxFeePolicy)
import           Pos.Core.Txp (Tx)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.V1.Types (unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, newOptions)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import           Cardano.Wallet.WalletLayer (EstimateFeesError (..),
                     NewPaymentError (..), RedeemAdaError (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Kernel.Conv

-- | Generates a new transaction @and submit it as pending@.
pay :: MonadIO m
    => Kernel.ActiveWallet
    -> PassPhrase
    -> InputGrouping
    -> ExpenseRegulation
    -> V1.Payment
    -> m (Either NewPaymentError (Tx, TxMeta))
pay activeWallet pw grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $
      runExceptT $ do
        (opts, accId, payees) <- withExceptT NewPaymentWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        withExceptT NewPaymentError $ ExceptT $
          Kernel.pay activeWallet pw opts accId payees

-- | Estimates the fees for a payment.
estimateFees :: MonadIO m
             => Kernel.ActiveWallet
             -> PassPhrase
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> m (Either EstimateFeesError Coin)
estimateFees activeWallet pw grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
      runExceptT $ do
        (opts, accId, payees) <- withExceptT EstimateFeesWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        withExceptT EstimateFeesError $ ExceptT $
          Kernel.estimateFees activeWallet pw opts accId payees

-- | Redeem an Ada voucher
--
-- Implementation note: No need for a time limit here, redemption does not run
-- coin selection.
redeemAda :: MonadIO m
          => Kernel.ActiveWallet
          -> V1.Redemption
          -> m (Either RedeemAdaError (Tx, TxMeta))
redeemAda aw
          V1.Redemption{
              redemptionRedemptionCode   = code
            , redemptionMnemonic         = mMnemonic
            , redemptionSpendingPassword = V1.V1 spendingPassword
            , redemptionWalletId         = wId
            , redemptionAccountIndex     = accIx
            } = runExceptT $ do
    accId <- withExceptT RedeemAdaWalletIdDecodingFailed $
               fromAccountId wId accIx
    case mMnemonic of
      Nothing -> do
        redeemKey <- withExceptT RedeemAdaInvalidRedemptionCode $
                       fromRedemptionCode code
        withExceptT RedeemAdaError $ ExceptT $ liftIO $
          Kernel.redeemAda aw accId spendingPassword redeemKey
      Just mnemonic -> do
        redeemKey <- withExceptT RedeemAdaInvalidRedemptionCode $
                       fromRedemptionCodePaper code mnemonic
        withExceptT RedeemAdaError $ ExceptT $ liftIO $
          Kernel.redeemAda aw accId spendingPassword redeemKey

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: Monad m
             => TxFeePolicy
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> ExceptT Text m ( CoinSelectionOptions
                               , HD.HdAccountId
                               , NonEmpty (Address, Coin)
                               )
setupPayment policy grouping regulation payment = do
    rootId <- fromRootId wId
    let opts   = (newOptions (Kernel.cardanoFee policy)) {
                     csoExpenseRegulation = regulation
                   , csoInputGrouping     = grouping
                   }
        accIx  = HD.HdAccountIx (V1.getAccIndex . V1.psAccountIndex . V1.pmtSource $ payment)
        accId  = HD.HdAccountId {
                     _hdAccountIdParent = rootId
                   , _hdAccountIdIx     = accIx
                   }
        payees = (\(V1.PaymentDistribution a c) -> (unV1 a, unV1 c)) <$>
                   V1.pmtDestinations payment
    return (opts, accId, payees)
  where
    wId = V1.psWalletId . V1.pmtSource $ payment
