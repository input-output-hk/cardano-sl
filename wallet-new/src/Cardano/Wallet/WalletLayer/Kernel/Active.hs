{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.WalletLayer.Kernel.Active (
    pay
  , estimateFees
  , createUnsignedTx
  , submitSignedTx
  , redeemAda
  ) where

import qualified Serokell.Util.Base16 as B16
import           Universum

import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Second)

import           Pos.Binary.Class (decodeFull')
import           Pos.Chain.Txp (Tx (..), TxSigData (..))
import           Pos.Core (Address, Coin, TxFeePolicy)
import           Pos.Crypto (PassPhrase, PublicKey, Signature (..))

import           Cardano.Crypto.Wallet (xsignature)
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
                     NewPaymentError (..), NewUnsignedTransactionError (..),
                     RedeemAdaError (..), SubmitSignedTransactionError (..))
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
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> m (Either EstimateFeesError Coin)
estimateFees activeWallet grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
      runExceptT $ do
        (opts, accId, payees) <- withExceptT EstimateFeesWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        withExceptT EstimateFeesError $ ExceptT $
          Kernel.estimateFees activeWallet opts accId payees

-- | Creates a raw transaction.
--
-- NOTE: this function does /not/ perform a payment, it just creates a new
-- transaction which will be signed and submitted to the blockchain later.
-- It returns a transaction and a list of source addresses with corresponding
-- derivation paths.
createUnsignedTx :: MonadIO m
                 => Kernel.ActiveWallet
                 -> InputGrouping
                 -> ExpenseRegulation
                 -> V1.Payment
                 -> m (Either NewUnsignedTransactionError V1.UnsignedTransaction)
createUnsignedTx activeWallet grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    let spendingPassword = maybe mempty coerce $ V1.pmtSpendingPassword payment
    res <- runExceptT $ do
        (opts, accId, payees) <- withExceptT NewTransactionWalletIdDecodingFailed $
            setupPayment policy grouping regulation payment
        withExceptT NewUnsignedTransactionError $ ExceptT $
            Kernel.prepareUnsignedTxWithSources activeWallet
                                                opts
                                                accId
                                                payees
                                                spendingPassword
    case res of
        Left e -> return $ Left e
        Right (tx, addrsAndPaths) -> do
            let txInHexFormat = V1.mkTransactionAsBase16 tx
                srcAddrsWithDerivationPaths = NE.toList $
                    NE.map (\(addr, path) -> V1.AddressAndPath (V1.mkAddressAsBase58 addr)
                                                               (map V1.word32ToAddressLevel path))
                           addrsAndPaths
            return $ Right $ V1.UnsignedTransaction txInHexFormat
                                                    srcAddrsWithDerivationPaths

-- | Submits externally-signed transaction to the blockchain.
submitSignedTx :: MonadIO m
               => Kernel.ActiveWallet
               -> V1.SignedTransaction
               -> m (Either SubmitSignedTransactionError (Tx, TxMeta))
submitSignedTx activeWallet (V1.SignedTransaction encodedTx encodedSrcAddrsWithProofs) = liftIO $
    case decodeTx of
        Left e -> pure (Left e)
        Right tx -> do
            let srcAddrsWithProofs = map decodeAddrAndProof encodedSrcAddrsWithProofs
                problems = lefts srcAddrsWithProofs
            if not . null $ problems then
                let (firstProblem:_) = problems in pure (Left firstProblem)
            else
                let validSrcAddrsWithProofs = rights srcAddrsWithProofs in
                runExceptT $ withExceptT SubmitSignedTransactionError $
                    ExceptT $ Kernel.submitSignedTx activeWallet
                                                    tx
                                                    (NE.fromList validSrcAddrsWithProofs)
  where
    decodeTx :: Either SubmitSignedTransactionError Tx
    decodeTx = case B16.decode (V1.rawTransactionAsBase16 encodedTx) of
        Left _ -> Left SubmitSignedTransactionNotBase16Format
        Right txAsBytes -> case decodeFull' txAsBytes of
            Left _           -> Left SubmitSignedTransactionUnableToDecode
            Right (tx :: Tx) -> pure tx

    decodeAddrAndProof :: V1.AddressWithProof
                       -> Either SubmitSignedTransactionError (Address, Signature TxSigData, PublicKey)
    decodeAddrAndProof (V1.AddressWithProof encSrcAddr encSig encDerivedPK) =
        case decodeSrcAddress encSrcAddr of
            Left e -> Left e
            Right srcAddress -> case decodeTxSig encSig of
                Left e -> Left e
                Right txSignature -> case decodeDerivedPK encDerivedPK of
                    Left e -> Left e
                    Right derivedPK -> pure (srcAddress, txSignature, derivedPK)
      where
        decodeSrcAddress encoded = case V1.mkAddressFromBase58 encoded of
            Left _        -> Left SubmitSignedTransactionInvalidSrcAddress
            Right srcAddr -> pure srcAddr

        decodeTxSig encoded = case B16.decode (V1.rawTransactionSignatureAsBase16 encoded) of
            Left _ -> Left SubmitSignedTransactionSigNotBase16Format
            Right txSigItself -> case xsignature txSigItself of
                Left _ -> Left SubmitSignedTransactionInvalidSig
                Right realTxSig -> pure (Signature realTxSig :: Signature TxSigData)

        decodeDerivedPK encoded = case V1.mkPublicKeyFromBase58 encoded of
            Left _          -> Left SubmitSignedTransactionInvalidPK
            Right derivedPK -> pure derivedPK

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
