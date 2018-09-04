{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cardano.Wallet.Kernel.Transactions (
      pay
    , estimateFees
    , redeemAda
    -- * Errors
    , NewTransactionError(..)
    , SignTransactionError(..)
    , PaymentError(..)
    , EstimateFeesError(..)
    , RedeemAdaError(..)
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
import qualified Pos.Client.Txp.Util as CTxp
import           Pos.Core (Address, Coin, TxFeePolicy (..), unsafeIntegerToCoin,
                     unsafeSubCoin)
import qualified Pos.Core as Core
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..), TxOut (..),
                     TxOutAux (..))
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, RedeemSecretKey,
                     SafeSigner (..), ShouldCheckPassphrase (..), hash,
                     redeemToPublic)

import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelFinalResult (..), CoinSelectionOptions,
                     estimateCardanoFee, estimateMaxTxInputs, mkStdTx)
import qualified Cardano.Wallet.Kernel.CoinSelection.FromGeneric as CoinSelection
import           Cardano.Wallet.Kernel.CoinSelection.Generic
                     (CoinSelHardErr (..))
import           Cardano.Wallet.Kernel.DB.AcidState (DB, NewForeignError,
                     NewPendingError)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Read as Getters
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal (ActiveWallet (..),
                     walletKeystore, walletNode)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.Pending (PartialTxMeta, newForeign,
                     newPending)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedTx (..), WalletId (..))
import           Cardano.Wallet.Kernel.Util (shuffleNE)
import           Cardano.Wallet.Kernel.Util.Core
import           Cardano.Wallet.WalletLayer.Kernel.Conv (exceptT,
                     toCardanoAddress)

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
                 Right (txAux, partialMeta, _utxo) -> do
                     succeeded <- newPending activeWallet accountId txAux partialMeta
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
                          Right meta -> return $ Right (taTx $ txAux, meta)
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
               -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newTransaction ActiveWallet{..} spendingPassword options accountId payees = runExceptT $ do
    initialEnv <- liftIO $ newEnvironment
    maxTxSize  <- liftIO $ Node.getMaxTxSize (walletPassive ^. walletNode)
    -- TODO: We should cache this maxInputs value
    let maxInputs = estimateMaxTxInputs maxTxSize

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
        mkTx        = mkStdTx walletProtocolMagic shuffleNE signAddress

    txAux <- withExceptT NewTransactionErrorSignTxFailed $ ExceptT $
               mkTx inputs outputs changeAddresses

    -- STEP 4: Compute metadata
    let txId = hash . taTx $ txAux
    -- This is the sum of inputs coins.
    let spentInputCoins = paymentAmount (toaOut . snd <$> inputs)
    -- partially applied, because we don`t know here which outputs are ours
    partialMeta <- liftIO $ createNewMeta accountId txId inputs (toaOut <$> outputs) True spentInputCoins
    return (txAux, partialMeta, availableUtxo)
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
        fmap toCardanoAddress <$>
            Kernel.createAddress spendingPassword
                                 (AccountIdHdRnd accountId)
                                 walletPassive

-- | This is called when we create a new Pending Transaction.
-- This actually returns a function because we don`t know yet our outputs.
createNewMeta :: HdAccountId -> TxId -> NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> Bool -> Coin -> IO PartialTxMeta
createNewMeta hdId txId inp out allInOurs spentInputsCoins = do
    time <- liftIO getCurrentTimestamp
    return $ metaForNewTx time hdId txId inp out allInOurs spentInputsCoins
    -- ^ this partially applied function indicates the lack of all TxMeta at this stage.

metaForNewTx  :: Core.Timestamp -> HdAccountId -> TxId -> NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> Bool -> Coin -> Bool -> Coin -> TxMeta
metaForNewTx time accountId txId inputs outputs allInpOurs spentInputsCoins allOutOurs gainedOutputsCoins =
    TxMeta {
          _txMetaId = txId
        , _txMetaAmount = absCoin spentInputsCoins gainedOutputsCoins
        , _txMetaInputs = inputsForMeta
        , _txMetaOutputs = aux <$> outputs
        , _txMetaCreationAt = time
        , _txMetaIsLocal = allInpOurs && allOutOurs
        , _txMetaIsOutgoing = gainedOutputsCoins < spentInputsCoins -- it`s outgoing if our inputs spent are more than the new utxo.
        , _txMetaWalletId = _fromDb $ getHdRootId (accountId ^. hdAccountIdParent)
        , _txMetaAccountIx = getHdAccountIx $ accountId ^. hdAccountIdIx
    }
  where
    inputsForMeta = toInput <$> inputs
    aux txOut = (txOutAddress txOut, txOutValue txOut)
    toInput (txin, txOutAux) = case txin of
        TxInUtxo txid index ->
            let (addr, coins) = aux $ toaOut txOutAux
            in (txid, index, addr, coins)
        TxInUnknown _ _ -> error "Tried to create TxMeta with unknown input"

-- | Different wraper for @metaForNewTx@ mainly for testing Only NewPending Transactions.
toMeta :: Core.Timestamp -> HdAccountId -> RawResolvedTx -> PartialTxMeta
toMeta time accountId UnsafeRawResolvedTx{..} allOutOurs outCoin =
    let allInpOurs = True
        txId = hash . taTx $ rawResolvedTx
        (txIn :: NonEmpty TxIn) = _txInputs $ taTx rawResolvedTx
        (inputsRes :: NonEmpty TxOutAux) = rawResolvedTxInputs
        spentInputCoins = paymentAmount $ toaOut <$> inputsRes
        inputs = NonEmpty.zip txIn inputsRes
        txOut = _txOutputs $ taTx rawResolvedTx
    in metaForNewTx time accountId txId inputs txOut allInpOurs spentInputCoins allOutOurs outCoin

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
      -- Unlike a block, a /single transaction/ cannot have inputs that sum to
      -- more than maxCoinVal
      sumOfInputs :: TxAux -> Utxo -> Coin
      sumOfInputs tx utxo =
          let inputs = Set.fromList $ toList . _txInputs . taTx $ tx
          in unsafeIntegerToCoin $
               utxoBalance (utxo `utxoRestrictToInputs` inputs)

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
cardanoFee :: TxFeePolicy -> Int -> NonEmpty Coin -> Coin
cardanoFee (TxFeePolicyTxSizeLinear policy) inputs outputs =
    Core.mkCoin $
      estimateCardanoFee policy inputs (toList $ fmap Core.getCoin outputs)
cardanoFee TxFeePolicyUnknown{} _ _ =
    error "cardanoFee: unknown policy"

{-------------------------------------------------------------------------------
  Ada redemption

  Old wallet layer implemention lives in "Pos.Wallet.Web.Methods.Redeem".
-------------------------------------------------------------------------------}

data RedeemAdaError =
    -- | Unknown account
    --
    -- NOTE: The Daedalus frontend requires users to create an account before
    -- they can redeem Ada.
    RedeemAdaUnknownAccountId UnknownHdAccount

    -- | We failed to translate the redeem key to a valid Cardano address
  | RedeemAdaErrorCreateAddressFailed Kernel.CreateAddressError

    -- | There are no UTxO available at the redeem address
    --
    -- This probably means that the voucher has already been redeemed, but it
    -- /could/ also mean that there was never any voucher at this address.
  | RedeemAdaNotAvailable Address

    -- | There are multiple outputs available at this address
    --
    -- This really should not happen at all. If this happens, we are running
    -- in a test setup with an invalid genesis block.
  | RedeemAdaMultipleOutputs Address

    -- | We were unable to submit the transaction
    --
    -- If this error happens, it almost certainly indicates a bug.
  | RedeemAdaNewForeignFailed NewForeignError

instance Buildable RedeemAdaError where
    build (RedeemAdaUnknownAccountId err) =
      bprint ("RedeemAdaUnknownAccountId " % build) err
    build (RedeemAdaErrorCreateAddressFailed err) =
      bprint ("RedeemAdaErrorCreateAddressFailed " % build) err
    build (RedeemAdaNotAvailable addr) =
      bprint ("RedeemAdaNotAvailable " % build) addr
    build (RedeemAdaMultipleOutputs addr) =
      bprint ("RedeemAdaMultipleOutputs " % build) addr
    build (RedeemAdaNewForeignFailed err) =
      bprint ("RedeemAdaNewForeignFailed " % build) err

-- | Redeem Ada voucher
--
-- NOTE: The account must already exist, it is /not/ created implicitly if it
-- does not yet exist.
redeemAda :: ActiveWallet
          -> HdAccountId      -- ^ Account ID
          -> PassPhrase       -- ^ Spending password
          -> RedeemSecretKey  -- ^ Redemption key
          -> IO (Either RedeemAdaError (Tx, TxMeta))
redeemAda w@ActiveWallet{..} accId pw rsk = runExceptT $ do
    snapshot   <- liftIO $ getWalletSnapshot walletPassive
    _accExists <- withExceptT RedeemAdaUnknownAccountId $ exceptT $
                    lookupHdAccountId snapshot accId
    changeAddr <- withExceptT RedeemAdaErrorCreateAddressFailed $ ExceptT $ liftIO $
                    Kernel.createAddress
                      pw
                      (AccountIdHdRnd accId)
                      walletPassive
    (tx, meta) <- mkTx (toCardanoAddress changeAddr)
    withExceptT RedeemAdaNewForeignFailed $ ExceptT $ liftIO $
      newForeign
        w
        accId
        tx
        meta
    return (taTx tx, meta)
  where
    redeemAddr :: Address
    redeemAddr = Core.makeRedeemAddress $ redeemToPublic rsk

    mkTx :: Address -> ExceptT RedeemAdaError IO (TxAux, TxMeta)
    mkTx output = do
        now  <- liftIO $ Core.getCurrentTimestamp
        utxo <- liftIO $
                  Node.withNodeState (walletPassive ^. walletNode) $ \_lock ->
                    Node.filterUtxo isOutput
        (inp@(TxInUtxo inHash inIx), coin) <-
          case utxo of
            [i]   -> return i
            []    -> throwError $ RedeemAdaNotAvailable    redeemAddr
            _:_:_ -> throwError $ RedeemAdaMultipleOutputs redeemAddr
        let out    = TxOutAux $ TxOut output coin
            txAux  = CTxp.makeRedemptionTx
                       walletProtocolMagic
                       rsk
                       (inp :| [])
                       (out :| [])
            txMeta = TxMeta {
                _txMetaId         = hash (taTx txAux)
              , _txMetaAmount     = coin
              , _txMetaInputs     = (inHash, inIx, redeemAddr, coin) :| []
              , _txMetaOutputs    = (output, coin) :| []
              , _txMetaCreationAt = now
              , _txMetaIsLocal    = False -- input does not belong to wallet
              , _txMetaIsOutgoing = False -- increases wallet's balance
              , _txMetaWalletId   = _fromDb $ getHdRootId (accId ^. hdAccountIdParent)
              , _txMetaAccountIx  = getHdAccountIx (accId ^. hdAccountIdIx)
              }
        return (txAux, txMeta)
      where
        isOutput :: (TxIn, TxOutAux) -> Maybe (TxIn, Coin)
        isOutput (inp, TxOutAux (TxOut addr coin)) = do
            guard $ addr == redeemAddr
            return (inp, coin)
