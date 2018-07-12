{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cardano.Wallet.Kernel.Transactions (
      pay
    , estimateFees
    -- * Errors
    , NewTransactionError(..)
    , PaymentError(..)
    , EstimateFeesError(..)
    , mkSigner
    , cardanoFee
    -- * Internal & testing use only
    , newTransaction
  ) where

import           Universum

import           Control.Lens (to)
import           Control.Retry (RetryPolicyM, RetryStatus, applyPolicy,
                     constantDelay, limitRetries, retrying)
import           Crypto.Random (MonadRandom (..))
import qualified Data.Set as Set
import           Test.QuickCheck (Arbitrary (..))

import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import qualified Data.ByteArray as ByteArray

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric (Cardano,
                     CoinSelectionOptions, estimateCardanoFee, mkStdTx)
import qualified Cardano.Wallet.Kernel.CoinSelection.FromGeneric as CoinSelection
import           Cardano.Wallet.Kernel.CoinSelection.Generic
                     (CoinSelHardErr (..))
import           Cardano.Wallet.Kernel.DB.AcidState (NewPendingError)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.HdWallet.Read
                     (readHdAddressByCardanoAddress)
import           Cardano.Wallet.Kernel.Util (paymentAmount, utxoBalance,
                     utxoRestrictToInputs)

import           Cardano.Wallet.Kernel (getWalletSnapshot, newPending)
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.Internal (ActiveWallet (..))

import           Pos.Core (Address, Coin, Tx (..), TxAux (..), TxOut (..),
                     TxOutAux (..), unsafeSubCoin)
import qualified Pos.Core as Core

import           Pos.Crypto (EncryptedSecretKey, PassPhrase, SafeSigner (..),
                     ShouldCheckPassphrase (..), hash)
import           Pos.Txp (Utxo)

{-------------------------------------------------------------------------------
  Generating payments and estimating fees
-------------------------------------------------------------------------------}

data NewTransactionError = CoinSelectionFailed CoinSelHardErr

instance Buildable NewTransactionError where
    build (CoinSelectionFailed hardErr) =
        bprint ("CoinSelectionFailed " % build) hardErr

instance Arbitrary NewTransactionError where
    arbitrary = CoinSelectionFailed <$> arbitrary

data PaymentError = PaymentNewTransactionError NewTransactionError
                  | PaymentNewPendingError NewPendingError
                  | PaymentSubmissionTimeoutReached
                  -- ^ When trying to send the newly-created transaction via
                  -- 'newPending' and the submission layer, we hit the number
                  -- of retries/max time allocated for the operation.

-- | Workhorse kernel function to perform a payment. It includes logic to
-- stop trying to perform a payment if the payment would take more than 30
-- seconds, as well as internally retrying up to 5 times to propagate the
-- transaction via 'newPending'.
pay :: ActiveWallet
    -> IO Address
    -- ^ A computation to generate change 'Address'es.
    -> (Address -> Either CoinSelHardErr SafeSigner)
    -> CoinSelectionOptions
    -> HdAccountId
    -- ^ The source @(root, account) from where the payment was
    -- originated
    -> NonEmpty (Address, Coin)
    -- ^ The payees
    -> IO (Either PaymentError Tx)
pay activeWallet genChangeAddr signAddress opts accountId payees = do
        retrying retryPolicy shouldRetry $ \rs -> do
            tx <- newTransaction activeWallet genChangeAddr signAddress opts accountId payees
            case tx of
                 Left e      -> return (Left $ PaymentNewTransactionError e)
                 Right txAux -> do
                     succeeded <- newPending activeWallet accountId txAux
                     case succeeded of
                          Left e   -> do
                              -- If the next retry would bring us to the
                              -- end of our allowed retries, we fail with
                              -- a proper error
                              retriesLeft <- applyPolicy retryPolicy rs
                              return . Left $ case retriesLeft of
                                   Nothing ->
                                       PaymentSubmissionTimeoutReached
                                   Just _  ->
                                       PaymentNewPendingError e
                          Right () -> return . Right . taTx $ txAux
    where
        retryPolicy :: RetryPolicyM IO
        retryPolicy = constantDelay 5000000 <> limitRetries 5

        -- If this is a hard coin selection error we cannot recover, stop
        -- retrying.
        shouldRetry :: RetryStatus -> Either PaymentError Tx -> IO Bool
        shouldRetry _ (Left (PaymentNewTransactionError _)) = return False
        shouldRetry _ _                                     = return True

-- | Creates a new 'TxAux' without submitting it to the network.
newTransaction :: ActiveWallet
               -> IO Address
               -- ^ A computation to generate change 'Address'es.
               -> (Address -> Either CoinSelHardErr SafeSigner)
               -- ^ A function to sign each Address
               -> CoinSelectionOptions
               -- ^ The options describing how to tune the coin selection.
               -> HdAccountId
               -- ^ The source @(root, account) from where the payment was
               -- originated
               -> NonEmpty (Address, Coin)
               -- ^ The payees
               -> IO (Either NewTransactionError TxAux)
newTransaction ActiveWallet{..} genChangeAddr signAddress options accountId payees = do
    snapshot <- getWalletSnapshot walletPassive
    let toTxOuts = fmap (\(a,c) -> TxOutAux (TxOut a c))
    -- We generate a new change address outside the 'MonadRandom' monad in which
    -- coin selection runs, so that we keep our \"external\" model consistent
    -- by generating it once, before calling coin selection. Specifically, we
    -- pass this potential change address from the outside.
    -- One disadvantage of this approach is that in case of failure of the
    -- coin-selection we might be left with such dangling address, but in the
    -- future we could easily add a function to delete an address, keeping the
    -- possible number of orphans at bay.
    let mkTx = mkStdTx walletProtocolMagic signAddress
    -- | FIXME(adn) This number was computed out of the work Matt Noonan did
    -- on the size estimation. See [CBR-318].
    let maxInputs = 350
    let availableUtxo = accountUtxo snapshot accountId
    res <- flip runReaderT payees . buildPayment $
           CoinSelection.random options
                                (liftIO genChangeAddr)
                                mkTx
                                maxInputs
                                (toTxOuts payees)
                                availableUtxo
    case res of
         Left err -> return . Left . CoinSelectionFailed $ err
         Right t  -> return . Right $ t

-- | Special monad used to process the payments, which randomness is derived
-- from a fixed seed obtained from hashing the payees. This guarantees that
-- when we estimate the fees and later create a transaction, the coin selection
-- will always yield the same value, making the process externally-predicatable.

newtype PayMonad a = PayMonad {
      buildPayment :: ReaderT (NonEmpty (Address, Coin)) IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (NonEmpty (Address, Coin)))

-- | \"Invalid\" 'MonadRandom' instance for 'PayMonad' which generates
-- randomness using the hash of 'NonEmpty (Address, Coin)' as fixed seed.
instance MonadRandom PayMonad where
    getRandomBytes _ =
        ask >>= return . ByteArray.convert
                       . encodeUtf8 @Text @ByteString
                       . sformat build
                       . hash

{-------------------------------------------------------------------------------
  Estimating fees
-------------------------------------------------------------------------------}

data EstimateFeesError = EstFeesTxCreationFailed NewTransactionError

instance Arbitrary EstimateFeesError where
    arbitrary = EstFeesTxCreationFailed <$> arbitrary

estimateFees :: ActiveWallet
             -> IO Address
             -- ^ A computation to generate a new change 'Address.
             -> (Address -> Either CoinSelHardErr SafeSigner)
             -- ^ A function to sign each Address
             -> CoinSelectionOptions
             -- ^ The options describing how to tune the coin selection.
             -> HdAccountId
             -- ^ The source @(root, account) from where the payment was
             -- originated
             -> NonEmpty (Address, Coin)
             -- ^ The payees
             -> IO (Either EstimateFeesError Coin)
estimateFees activeWallet@ActiveWallet{..} genChangeAddr signAddress options accountId payees = do
    snapshot         <- getWalletSnapshot walletPassive
    let originalUtxo = accountUtxo snapshot accountId
    res <- newTransaction activeWallet genChangeAddr signAddress options accountId payees
    case res of
         Left e  -> return . Left . EstFeesTxCreationFailed $ e
         Right tx -> -- calculate the fee as the difference between inputs and outputs.
             -- NOTE(adn) Do we need to worry about the 'ExpenseRegulation'
             -- affecting the way we sum?
             return $ Right
                    $ sumOfInputs tx originalUtxo `unsafeSubCoin` sumOfOutputs tx
  where
      sumOfInputs :: TxAux -> Utxo -> Coin
      sumOfInputs tx utxo =
          let inputs = Set.fromList $ toList . _txInputs . taTx $ tx
          in utxoBalance (utxoRestrictToInputs inputs utxo)

      sumOfOutputs :: TxAux -> Coin
      sumOfOutputs tx =
          let outputs = _txOutputs . taTx $ tx
          in paymentAmount outputs


-- NOTE(adn) At the moment we are passing
-- the full set of Hd wallets as input, which means our lookup function
-- for an 'HdAddress' can span across the whole wallets and can be very
-- costly. However, in the way the 'DB' is modelled at the moment, we
-- store the addresses in a \"flat\" representation (i.e. in an 'IxSet'),
-- so indexing directly by 'Core.Address' might not be any less efficient
-- than performing two lookups, one on 'HdAccountId' followed by one on
-- 'Core.Address'.
mkSigner :: PassPhrase
         -> Maybe EncryptedSecretKey
         -> HD.HdWallets
         -> Address
         -> Either CoinSelHardErr SafeSigner
mkSigner _ Nothing _ addr = Left (CoinSelHardErrAddressNotOwned (Proxy @ Cardano) addr)
mkSigner spendingPassword (Just esk) allWallets addr =
    case readHdAddressByCardanoAddress addr allWallets of
        Left _ -> Left (CoinSelHardErrAddressNotOwned (Proxy @ Cardano) addr)
        Right hdAddr ->
            let addressIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdIx
                                       . to HD.getHdAddressIx
                accountIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdParent
                                       . HD.hdAccountIdIx
                                       . to HD.getHdAccountIx
                res = Core.deriveLvl2KeyPair (Core.IsBootstrapEraAddr True)
                                             (ShouldCheckPassphrase False)
                                             spendingPassword
                                             esk
                                             accountIndex
                                             addressIndex
            in case res of
                 Just (a, _) | a == addr ->
                     Right (SafeSigner esk spendingPassword)
                 _                   ->
                     Left (CoinSelHardErrAddressNotOwned (Proxy @ Cardano) addr)

-- | An hopefully-accurate estimate of the Tx fees in Cardano.
cardanoFee :: Int -> NonEmpty Coin -> Coin
cardanoFee inputs outputs = Core.mkCoin $
    estimateCardanoFee linearFeePolicy inputs (toList $ fmap Core.getCoin outputs)
    where
      linearFeePolicy = Core.TxSizeLinear (Core.Coeff 155381) (Core.Coeff 43.946)
