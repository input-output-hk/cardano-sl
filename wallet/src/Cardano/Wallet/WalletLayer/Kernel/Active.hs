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
import           Pos.Core (AddrAttributes (..), Address (..), Coin, TxFeePolicy)
import           Pos.Core.Attributes (Attributes (..))
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
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
import           Cardano.Wallet.Kernel.Internal (walletProtocolMagic)
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

        -- Verify that all payee addresses are of the same `NetworkMagic`
        -- as our `ActiveWallet`.
        let nm = makeNetworkMagic $ Kernel.walletPassive activeWallet ^. walletProtocolMagic
        ExceptT $ pure $ verifyPayeesNM nm payees

        -- Pay the payees
        withExceptT NewPaymentError $ ExceptT $
            Kernel.pay activeWallet pw opts accId payees

-- | Verifies that the `NetworkMagic` of each payee address matches the
-- provided `NetworkMagic`.
verifyPayeesNM
    :: NetworkMagic
    -> NonEmpty (Address, Coin)
    -> Either NewPaymentError ()
verifyPayeesNM nm payees =
    case nonEmpty invalidPayees of
        Nothing -> Right ()
        Just is -> Left $ NewPaymentAddressBadNetworkMagic nm is
  where
    addressHasValidMagic :: AddrAttributes -> Bool
    addressHasValidMagic addrAttrs = nm == (aaNetworkMagic addrAttrs)
    --
    verifyPayeeNM
        :: (Address, Coin)
        -> Either Address ()
    verifyPayeeNM (addr, _)
        | (addressHasValidMagic ((attrData . addrAttributes) addr)) = Right ()
        | otherwise = Left addr
    --
    invalidPayees :: [Address]
    invalidPayees = fst $ partitionEithers (toList (map verifyPayeeNM payees))

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
                    NE.map (\(addr, path) -> V1.AddressAndPath (V1.V1 addr)
                                                               (map V1.word32ToAddressLevel path))
                           addrsAndPaths
            return $ Right $ V1.UnsignedTransaction txInHexFormat
                                                    srcAddrsWithDerivationPaths

-- | Submits externally-signed transaction to the blockchain.
submitSignedTx :: MonadIO m
               => Kernel.ActiveWallet
               -> V1.SignedTransaction
               -> m (Either SubmitSignedTransactionError (Tx, TxMeta))
submitSignedTx activeWallet (V1.SignedTransaction encodedTx encodedSrcAddrsWithProofs) = runExceptT $ do
    txAsBytes <- withExceptT (const SubmitSignedTransactionNotBase16Format) $ ExceptT $
        pure $ B16.decode (V1.rawTransactionAsBase16 encodedTx)
    tx :: Tx <- withExceptT (const SubmitSignedTransactionUnableToDecode) $ ExceptT $
        pure $ decodeFull' txAsBytes

    srcAddrsWithProofs <- mapM decodeAddrAndProof encodedSrcAddrsWithProofs
    let problems = lefts srcAddrsWithProofs
    if not . null $ problems then
        -- Something is wrong with proofs, take the first problem we know about.
        let (firstProblem:_) = problems in
        ExceptT $ pure $ Left firstProblem
    else
        let validSrcAddrsWithProofs = rights srcAddrsWithProofs in
        withExceptT SubmitSignedTransactionError $ ExceptT $ liftIO $
            Kernel.submitSignedTx activeWallet
                                  tx
                                  (NE.fromList validSrcAddrsWithProofs)
  where
    decodeAddrAndProof :: Monad m
                       => V1.AddressWithProof
                       -> m (Either SubmitSignedTransactionError (Address, Signature TxSigData, PublicKey))
    decodeAddrAndProof (V1.AddressWithProof srcAddr encSig derivedPK) = runExceptT $ do
        txSigItself <- withExceptT (const SubmitSignedTransactionSigNotBase16Format) $ ExceptT $
            pure $ B16.decode (V1.rawTransactionSignatureAsBase16 encSig)
        realTxSig <- withExceptT (const SubmitSignedTransactionInvalidSig) $ ExceptT $
            pure $ xsignature txSigItself
        let txSignature = Signature realTxSig :: Signature TxSigData
        ExceptT $ pure $ Right (unV1 srcAddr, txSignature, derivedPK)

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
    let opts   = (newOptions (Kernel.cardanoFee policy) (Kernel.cardanoFeeSanity policy)) {
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
