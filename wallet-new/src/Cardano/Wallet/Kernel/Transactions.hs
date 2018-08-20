{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cardano.Wallet.Kernel.Transactions (
      pay
    , estimateFees
    -- * Errors
    , NewTransactionError(..)
    , SignTransactionError(..)
    , PaymentError(..)
    , EstimateFeesError(..)
    , cardanoFee
    -- * Internal & testing use only
    , newTransaction
    , toMeta
  ) where

import           Universum

import           Control.Lens (to)
import           Control.Monad.Except (MonadError (..), withExceptT)
import           Control.Retry (RetryPolicyM, RetryStatus, applyPolicy,
                     fullJitterBackoff, limitRetries, retrying)
import           Crypto.Random (MonadRandom (..))
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Vector as V
import           System.Random.MWC (GenIO, asGenIO, initialize, uniformVector)
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Address, Coin, unsafeSubCoin)
import qualified Pos.Core as Core
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, SafeSigner (..),
                     ShouldCheckPassphrase (..), hash)

import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelFinalResult (..), CoinSelectionOptions,
                     dummyAddrAttrSize, dummyTxAttrSize, estimateCardanoFee,
                     estimateMaxTxInputs, mkStdTx)
import qualified Cardano.Wallet.Kernel.CoinSelection.FromGeneric as CoinSelection
import           Cardano.Wallet.Kernel.CoinSelection.Generic
                     (CoinSelHardErr (..))
import           Cardano.Wallet.Kernel.DB.AcidState (DB, NewPendingError)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal (ActiveWallet (..),
                     walletKeystore, walletNode)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (getMaxTxSize)
import           Cardano.Wallet.Kernel.Pending (newPending)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedTx (..), WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core (paymentAmount, utxoBalance,
                     utxoRestrictToInputs)
import           Cardano.Wallet.WalletLayer.Kernel.Conv (exceptT)

{-------------------------------------------------------------------------------
  Generating payments and estimating fees
-------------------------------------------------------------------------------}

data NewTransactionError =
    NewTransactionUnknownAccount UnknownHdAccount
  | NewTransactionErrorCoinSelectionFailed CoinSelHardErr
  | NewTransactionErrorCreateAddressFailed Kernel.CreateAddressError
  | NewTransactionErrorSignTxFailed SignTransactionError
  | NewTransactionInvalidTxIn

instance Buildable NewTransactionError where
    build (NewTransactionUnknownAccount err) =
        bprint ("NewTransactionUnknownAccount " % build) err
    build (NewTransactionErrorCoinSelectionFailed err) =
        bprint ("NewTransactionErrorCoinSelectionFailed " % build) err
    build (NewTransactionErrorCreateAddressFailed err) =
        bprint ("NewTransactionErrorCreateAddressFailed " % build) err
    build (NewTransactionErrorSignTxFailed err) =
        bprint ("NewTransactionErrorSignTxFailed " % build) err
    build NewTransactionInvalidTxIn =
        bprint "NewTransactionInvalidTxIn"

instance Arbitrary NewTransactionError where
    arbitrary = oneof [
        NewTransactionUnknownAccount <$> arbitrary
      , NewTransactionErrorCoinSelectionFailed <$> arbitrary
      , NewTransactionErrorCreateAddressFailed <$> arbitrary
      , NewTransactionErrorSignTxFailed <$> arbitrary
      , pure NewTransactionInvalidTxIn
      ]

data PaymentError = PaymentNewTransactionError NewTransactionError
                  | PaymentNewPendingError NewPendingError
                  | PaymentSubmissionMaxAttemptsReached
                  -- ^ When trying to send the newly-created transaction via
                  -- 'newPending' and the submission layer, we hit the number
                  -- of retries/max time allocated for the operation.

instance Buildable PaymentError where
    build (PaymentNewTransactionError txErr) =
        bprint ("PaymentNewTransactionError " % build) txErr
    build (PaymentNewPendingError npe) =
        bprint ("PaymentNewPendingError " % build) npe
    build PaymentSubmissionMaxAttemptsReached =
        bprint "PaymentSubmissionMaxAttemptsReached"

-- | Workhorse kernel function to perform a payment. It includes logic to
-- stop trying to perform a payment if the payment would take more than 30
-- seconds, as well as internally retrying up to 5 times to propagate the
-- transaction via 'newPending'.
pay :: ActiveWallet
    -> PassPhrase
    -> CoinSelectionOptions
    -> HdAccountId
    -- ^ The source HD Account from where the payment was originated
    -> NonEmpty (Address, Coin)
    -- ^ The payees
    -> IO (Either PaymentError (Tx, TxMeta))
pay activeWallet spendingPassword opts accountId payees = do
        retrying retryPolicy shouldRetry $ \rs -> do
            res <- newTransaction activeWallet spendingPassword opts accountId payees
            case res of
                 Left e      -> return (Left $ PaymentNewTransactionError e)
                 Right (txAux, meta, _utxo) -> do
                     succeeded <- newPending activeWallet accountId txAux (Just meta)
                     case succeeded of
                          Left e   -> do
                              -- If the next retry would bring us to the
                              -- end of our allowed retries, we fail with
                              -- a proper error
                              retriesLeft <- applyPolicy retryPolicy rs
                              return . Left $ case retriesLeft of
                                   Nothing ->
                                       PaymentSubmissionMaxAttemptsReached
                                   Just _  ->
                                       PaymentNewPendingError e
                          Right () -> return $ Right (taTx $ txAux, meta)
    where
        -- See <https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter>
        retryPolicy :: RetryPolicyM IO
        retryPolicy = fullJitterBackoff 5000000 <> limitRetries 6

        -- If this is a hard coin selection error we cannot recover, stop
        -- retrying. If we get a 'Tx' as output, stop retrying immediately.
        shouldRetry :: RetryStatus -> Either PaymentError a -> IO Bool
        shouldRetry _ (Right _)                             = return False
        shouldRetry _ (Left (PaymentNewTransactionError _)) = return False
        shouldRetry _ _                                     = return True

-- | Creates a new 'TxAux' and corresponding 'TxMeta',
-- without submitting it to the network.
--
-- For testing purposes, if successful this additionally returns the utxo
-- that coin selection was run against.
newTransaction :: ActiveWallet
               -> PassPhrase
               -- ^ The spending password.
               -> CoinSelectionOptions
               -- ^ The options describing how to tune the coin selection.
               -> HdAccountId
               -- ^ The source HD account from where the payment should originate
               -> NonEmpty (Address, Coin)
               -- ^ The payees
               -> IO (Either NewTransactionError (TxAux, TxMeta, Utxo))
newTransaction ActiveWallet{..} spendingPassword options accountId payees = runExceptT $ do
    initialEnv <- liftIO $ newEnvironment
    maxTxSize  <- liftIO $ getMaxTxSize (walletPassive ^. walletNode)
    let maxInputs = estimateMaxTxInputs dummyAddrAttrSize dummyTxAttrSize maxTxSize

    -- STEP 0: Get available UTxO
    snapshot      <- liftIO $ getWalletSnapshot walletPassive
    availableUtxo <- withExceptT NewTransactionUnknownAccount $ exceptT $
                       currentAvailableUtxo snapshot accountId

    -- STEP 1: Run coin selection.
    CoinSelFinalResult inputs outputs coins <-
      withExceptT NewTransactionErrorCoinSelectionFailed $ ExceptT $
        flip runReaderT initialEnv . buildPayment $
          CoinSelection.random options
                               maxInputs
                               (fmap toTxOut payees)
                               availableUtxo

    -- STEP 2: Generate the change addresses needed.
    changeAddresses <- withExceptT NewTransactionErrorCreateAddressFailed $
                         genChangeOuts coins

    -- STEP 3: Perform the signing and forge the final TxAux.
    mbEsk <- liftIO $ Keystore.lookup
               (WalletIdHdRnd $ accountId ^. hdAccountIdParent)
               (walletPassive ^. walletKeystore)
    let signAddress = mkSigner spendingPassword mbEsk snapshot
        mkTx        = mkStdTx walletProtocolMagic signAddress

    txAux <- withExceptT NewTransactionErrorSignTxFailed $ ExceptT $
               mkTx inputs outputs changeAddresses

    -- STEP 4: Compute metadata
    let txId = hash . taTx $ txAux
    txMeta <- createNewMeta accountId txId inputs (toaOut <$> outputs)
    return (txAux, txMeta, availableUtxo)
  where
        -- Generate an initial seed for the random generator using the hash of
        -- the payees, which ensure that the coin selection (and the fee estimation)
        -- is \"pseudo deterministic\" and replicable.
        newEnvironment :: IO Env
        newEnvironment =
            let initialSeed = V.fromList . map fromIntegral
                                         . B.unpack
                                         . encodeUtf8 @Text @ByteString
                                         . sformat build
                                         $ hash payees
            in Env <$> initialize initialSeed

        toTxOut :: (Address, Coin) -> TxOutAux
        toTxOut (a, c) = TxOutAux (TxOut a c)

        -- | Generates the list of change outputs from a list of change coins.
        genChangeOuts :: MonadIO m
                      => [Coin]
                      -> ExceptT Kernel.CreateAddressError m [TxOutAux]
        genChangeOuts css = forM css $ \change -> do
            changeAddr <- genChangeAddr
            return TxOutAux {
                toaOut = TxOut {
                    txOutAddress = changeAddr
                  , txOutValue   = change
                  }
              }

        -- | Monadic computation to generate a new change 'Address'. This will
        -- run after coin selection, when we create the final transaction as
        -- part of 'mkTx'.
        genChangeAddr :: MonadIO m
                      => ExceptT Kernel.CreateAddressError m Address
        genChangeAddr = ExceptT $ liftIO $
            Kernel.createAddress spendingPassword
                                 (AccountIdHdRnd accountId)
                                 walletPassive

createNewMeta :: HdAccountId -> TxId -> NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> ExceptT NewTransactionError IO TxMeta
createNewMeta hdId txId inp out = do
    time <- Core.getCurrentTimestamp
    metaForNewTx time hdId txId inp out

metaForNewTx :: (Monad m) => Core.Timestamp -> HdAccountId -> TxId -> NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> ExceptT NewTransactionError m TxMeta
metaForNewTx time accountId txId inputs outputs = do
    inputsForMeta <- forM inputs toInput
    return TxMeta {
                  _txMetaId = txId
                , _txMetaAmount = minBound -- TODO(kde): find out what this should be. |sum(o) - sum(i)| maybe?
                , _txMetaInputs = inputsForMeta
                , _txMetaOutputs = aux <$> outputs
                , _txMetaCreationAt = time
                , _txMetaIsLocal = False -- TODO(kde): find a way to check if all addresses are ours.
                , _txMetaIsOutgoing = True
                , _txMetaWalletId = _fromDb $ getHdRootId (accountId ^. hdAccountIdParent)
                , _txMetaAccountIx = getHdAccountIx $ accountId ^. hdAccountIdIx
            }
  where
    aux txOut = (txOutAddress txOut, txOutValue txOut)
    toInput (txin, txOutAux) = case txin of
        TxInUtxo txid index ->
            let (addr, coins) = aux $ toaOut txOutAux
            in return (txid, index, addr, coins)
        TxInUnknown _ _ -> throwError NewTransactionInvalidTxIn

toMeta :: Monad m => Core.Timestamp -> HdAccountId -> RawResolvedTx -> m (Either NewTransactionError TxMeta)
toMeta time accountId UnsafeRawResolvedTx{..} = do
    let txId = hash . taTx $ rawResolvedTx
        (txIns :: NonEmpty TxIn) = _txInputs $ taTx rawResolvedTx
        (inputsRes :: NonEmpty TxOutAux) = rawResolvedTxInputs
        inputs = NonEmpty.zip txIns inputsRes
        txOuts = _txOutputs $ taTx rawResolvedTx
    runExceptT $ metaForNewTx time accountId txId inputs txOuts

-- | Special monad used to process the payments, which randomness is derived
-- from a fixed seed obtained from hashing the payees. This guarantees that
-- when we estimate the fees and later create a transaction, the coin selection
-- will always yield the same value, making the process externally-predicatable.
newtype PayMonad a = PayMonad {
      buildPayment :: ReaderT Env IO a
    } deriving ( Functor , Applicative , Monad , MonadIO, MonadReader Env)

-- | This 'Env' datatype is necessary to convince GHC that indeed we have
-- a 'MonadReader' instance defined on 'GenIO' for the 'PayMonad'.
newtype Env = Env { getEnv :: GenIO }

-- | \"Invalid\" 'MonadRandom' instance for 'PayMonad' which generates
-- randomness using the hash of 'NonEmpty (Address, Coin)' as fixed seed,
-- plus an internal counter used to shift the bits of such hash.
-- This ensures that the coin selection algorithm runs in a random environment
-- which is yet deterministically reproduceable by feeding the same set of
-- payees.
instance MonadRandom PayMonad where
    getRandomBytes len = do
        gen <- asks getEnv
        randomBytes <- liftIO (asGenIO (flip uniformVector len) gen)
        return $ ByteArray.convert (B.pack $ V.toList randomBytes)

{-------------------------------------------------------------------------------
  Estimating fees
-------------------------------------------------------------------------------}

data EstimateFeesError = EstFeesTxCreationFailed NewTransactionError

instance Buildable EstimateFeesError where
    build (EstFeesTxCreationFailed newTxErr) =
        bprint ("EstFeesTxCreationFailed " % build) newTxErr

instance Arbitrary EstimateFeesError where
    arbitrary = EstFeesTxCreationFailed <$> arbitrary

estimateFees :: ActiveWallet
             -> PassPhrase
             -- ^ The spending password.
             -> CoinSelectionOptions
             -- ^ The options describing how to tune the coin selection.
             -> HdAccountId
             -- ^ The source HD Account from where the payment should originate
             -> NonEmpty (Address, Coin)
             -- ^ The payees
             -> IO (Either EstimateFeesError Coin)
estimateFees activeWallet@ActiveWallet{..} spendingPassword options accountId payees = do
    res <- newTransaction activeWallet spendingPassword options accountId payees
    case res of
         Left e  -> return . Left . EstFeesTxCreationFailed $ e
         Right (tx, _txMeta, originalUtxo) ->
             -- calculate the fee as the difference between inputs and outputs.
             -- NOTE(adn) In case of 'SenderPaysFee' is practice there might be a slightly
             -- increase of the projected fee in the case we are forced to pick "yet another input"
             -- to be able to pay the fee, which would, in turn, also increase the fee due to
             -- the extra input being picked.
             return $ Right
                    $ sumOfInputs tx originalUtxo `unsafeSubCoin` sumOfOutputs tx
  where
      sumOfInputs :: TxAux -> Utxo -> Coin
      sumOfInputs tx utxo =
          let inputs = Set.fromList $ toList . _txInputs . taTx $ tx
          in utxoBalance (utxo `utxoRestrictToInputs` inputs)

      sumOfOutputs :: TxAux -> Coin
      sumOfOutputs tx =
          let outputs = _txOutputs . taTx $ tx
          in paymentAmount outputs

-- | Errors during transaction signing
--
-- NOTE: Under normal circumstances these should /never/ be thrown. If they
-- do, it most likely indicates a bug.
data SignTransactionError =
    SignTransactionMissingKey Address
  | SignTransactionErrorUnknownAddress Address
  | SignTransactionErrorNotOwned Address

instance Buildable SignTransactionError where
    build (SignTransactionMissingKey addr) =
        bprint ("SignTransactionMissingKey " % build) addr
    build (SignTransactionErrorUnknownAddress addr) =
        bprint ("SignTransactionErrorUnknownAddress " % build) addr
    build (SignTransactionErrorNotOwned addr) =
        bprint ("SignTransactionErrorNotOwned " % build) addr

-- in order to be able to generate an Arbitrary address we'd need to use
-- the cardano-sl-core test package
instance Arbitrary SignTransactionError where
    arbitrary = oneof []

mkSigner :: PassPhrase
         -> Maybe EncryptedSecretKey
         -> DB
         -> Address
         -> Either SignTransactionError SafeSigner
mkSigner _ Nothing _ addr = Left (SignTransactionMissingKey addr)
mkSigner spendingPassword (Just esk) snapshot addr =
    case Getters.lookupCardanoAddress snapshot addr of
        Left _ -> Left (SignTransactionErrorUnknownAddress addr)
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
                 _otherwise              ->
                     Left (SignTransactionErrorNotOwned addr)

-- | An estimate of the Tx fees in Cardano based on a sensible number of defaults.
cardanoFee :: Int -> NonEmpty Coin -> Coin
cardanoFee inputs outputs = Core.mkCoin $
    estimateCardanoFee linearFeePolicy inputs (toList $ fmap Core.getCoin outputs)
    where
      linearFeePolicy = Core.TxSizeLinear (Core.Coeff 155381) (Core.Coeff 43.946)
