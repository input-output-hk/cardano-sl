{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
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
    , cardanoFeeSanity
    , mkStdTx
    , prepareUnsignedTxWithSources
    , submitSignedTx
    -- * Internal & testing use only low-level APIs
    , newTransaction
    , newUnsignedTransaction
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
import           Data.Default (def)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified System.Random.MWC (GenIO, asGenIO, initialize, uniformVector)
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

import           Cardano.Crypto.Wallet (DerivationIndex)
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelFinalResult (..), CoinSelectionOptions (..),
                     checkCardanoFeeSanity, estimateCardanoFee,
                     estimateMaxTxInputs)
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
                     PassiveWallet (..), walletNode)
import qualified Cardano.Wallet.Kernel.Internal as Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.Pending (PartialTxMeta, newForeign,
                     newPending)
import           Cardano.Wallet.Kernel.Read (getWalletSnapshot)
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedTx (..), WalletId (..))
import           Cardano.Wallet.Kernel.Util.Core
import           Cardano.Wallet.WalletLayer.Kernel.Conv (exceptT)
import           Pos.Chain.Txp (Tx (..), TxAttributes, TxAux (..), TxId,
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), Utxo)
import           Pos.Chain.Txp as Core (TxAttributes, TxAux, TxIn, TxOut,
                     TxOutAux, toaOut, txOutAddress, txOutValue)
import qualified Pos.Client.Txp.Util as CTxp
import           Pos.Core (Address, Coin, TxFeePolicy (..), unsafeSubCoin)
import qualified Pos.Core as Core
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic,
                     PublicKey, RedeemSecretKey, SafeSigner (..),
                     ShouldCheckPassphrase (..), Signature (..), hash,
                     redeemToPublic)
import           UTxO.Util (shuffleNE)

{-------------------------------------------------------------------------------
  Generating payments and estimating fees
-------------------------------------------------------------------------------}

data NewTransactionError =
    NewTransactionUnknownAccount UnknownHdAccount
  | NewTransactionUnknownAddress UnknownHdAddress
  | NewTransactionErrorCoinSelectionFailed CoinSelHardErr
  | NewTransactionErrorCreateAddressFailed Kernel.CreateAddressError
  | NewTransactionErrorSignTxFailed SignTransactionError
  | NewTransactionInvalidTxIn

instance Buildable NewTransactionError where
    build (NewTransactionUnknownAccount err) =
        bprint ("NewTransactionUnknownAccount " % build) err
    build (NewTransactionUnknownAddress err) =
        bprint ("NewTransactionUnknownAddress " % build) err
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
      , NewTransactionErrorCoinSelectionFailed <$> oneof
            [ pure $ CoinSelHardErrUtxoExhausted "0 coin(s)" "14 coin(s)"
            , pure CoinSelHardErrCannotCoverFee
            ]
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
                  | PaymentNoHdAddressForSrcAddress HD.UnknownHdAddress
                  -- ^ When we don't have HD-address corresponding to source
                  -- address of transaction.

instance Buildable PaymentError where
    build (PaymentNewTransactionError txErr) =
        bprint ("PaymentNewTransactionError " % build) txErr
    build (PaymentNewPendingError npe) =
        bprint ("PaymentNewPendingError " % build) npe
    build PaymentSubmissionMaxAttemptsReached =
        bprint "PaymentSubmissionMaxAttemptsReached"
    build (PaymentNoHdAddressForSrcAddress addrErr) =
        bprint ("PaymentNoHdAddressForSrcAddress" % build) addrErr

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

-- See <https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter>
retryPolicy :: RetryPolicyM IO
retryPolicy = fullJitterBackoff 5000000 <> limitRetries 6

-- If this is a hard coin selection error we cannot recover, stop
-- retrying. If we get a 'Tx' as output, stop retrying immediately.
shouldRetry :: RetryStatus -> Either PaymentError a -> IO Bool
shouldRetry _ (Right _)                             = return False
shouldRetry _ (Left (PaymentNewTransactionError _)) = return False
shouldRetry _ _                                     = return True

{-----------------------------------------------------------------------------
  Creating transactions (low-level API)
------------------------------------------------------------------------------}

-- | Creates a new unsigned 'Tx', without submitting it to the network. Please
-- note that this is a low-level API. Considering using 'pay' and 'estimateFee'
-- in user's code.
--
-- For testing purposes, if successful this additionally returns the utxo
-- that coin selection was run against.
newUnsignedTransaction
    :: ActiveWallet
    -> CoinSelectionOptions
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate
    -> NonEmpty (Address, Coin)
    -- ^ The payees
    -> IO (Either NewTransactionError (DB, UnsignedTx, Coin, Utxo))
    -- ^ Returns the state of the world (i.e. the DB snapshot)
    -- at the time of the coin selection, so that it can later
    -- on be used to sign the addresses.
newUnsignedTransaction ActiveWallet{..} options accountId payees = runExceptT $ do
    snapshot <- liftIO $ getWalletSnapshot walletPassive
    initialEnv <- liftIO $ newEnvironment
    maxTxSize  <- liftIO $ Node.getMaxTxSize (walletPassive ^. walletNode)
    -- TODO: We should cache this maxInputs value
    let maxInputs = estimateMaxTxInputs maxTxSize

    -- STEP 0: Get available UTxO
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

    -- STEP 2: Assemble the unsigned transactions, @without@ generating the
    -- change addresses, as that would require the spending password.
    -- Currently all transactions has default (empty) attributes. Please note
    -- that it may change in the future.
    let attributes = def :: TxAttributes
    let tx = UnsignedTx inputs outputs attributes coins

    -- STEP 3: Sanity test. Here we check whether our fees are within a reasonable
    -- range.
    let fees = computeFeesOfUnsignedTx tx
    if csoFeesSanityCheck options fees
    then return (snapshot, tx, fees, availableUtxo)
    else error $ "fees out of bound " <> show fees
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
        in Env <$> System.Random.MWC.initialize initialSeed

    toTxOut :: (Address, Coin) -> TxOutAux
    toTxOut (a, c) = TxOutAux (TxOut a c)

-- | Creates a new unsigned transaction.
--
-- NOTE: this function does /not/ perform a payment, it just creates a new
-- transaction which will be signed and submitted to the blockchain later.
-- It returns a transaction and a list of source addresses with corresponding
-- derivation paths.
prepareUnsignedTxWithSources
    :: ActiveWallet
    -> CoinSelectionOptions
    -- ^ The options describing how to tune the coin selection.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> NonEmpty (Address, Coin)
    -- ^ The payees.
    -> PassPhrase
    -> IO (Either
             NewTransactionError
             (Tx, NonEmpty (Address, [DerivationIndex]))
          )
prepareUnsignedTxWithSources activeWallet opts srcAccountId payees spendingPassword = runExceptT $ do
    (db, unsignedTx, _fees, _availableUtxo) <- ExceptT $
        newUnsignedTransaction activeWallet opts srcAccountId payees

    -- Now we have to generate the change addresses needed,
    -- because 'newUnsignedTransaction' function cannot do it by itself.
    changeAddresses <- withExceptT NewTransactionErrorCreateAddressFailed $
        genChangeOuts (unsignedTxChange unsignedTx)
                      srcAccountId
                      spendingPassword
                      (walletPassive activeWallet)
    -- We have to provide source addresses and derivation paths for this transaction.
    -- It will be used by external party to provide a proof that it has a right to
    -- spend this money.
    let tx = UnsafeTx
              (map fst $ unsignedTxInputs unsignedTx)
              (map toaOut $ appendList (unsignedTxOutputs unsignedTx) changeAddresses)
              (unsignedTxAttributes unsignedTx)
    ExceptT $ return $ case mapM (prepareSourceAddress db) $ unsignedTxInputs unsignedTx of
        Left err  -> Left err
        Right res -> Right $ (tx, res)
  where
    appendList :: NonEmpty a -> [a] -> NonEmpty a
    appendList = foldl' (flip NonEmpty.cons)

    prepareSourceAddress
        :: DB -> (Core.TxIn, Core.TxOutAux)
        -> Either NewTransactionError (Address, [DerivationIndex])
    prepareSourceAddress db addr
        = case lookupCardanoAddress db address of
            Left  err -> Left $ NewTransactionUnknownAddress err
            Right res -> Right (address, [accountIndex, getAddressIndex res])
      where
        address = txOutAddress $ toaOut $ snd addr
        accountIndex = getHdAccountIx  $ _hdAccountIdIx srcAccountId
        getAddressIndex = getHdAddressIx . _hdAddressIdIx . _hdAddressId

-- | Submits externally-signed transaction to the blockchain.
-- The result of this function is equal to the result of 'pay' function.
submitSignedTx
    :: ActiveWallet
    -> Tx
    -> NonEmpty (Address, Signature TxSigData, PublicKey)
    -> IO (Either PaymentError (Tx, TxMeta))
submitSignedTx aw@ActiveWallet{..} tx srcAddrsWithProofs =
    retrying retryPolicy shouldRetry $ \rs -> do
        res <- runExceptT $ do
            -- STEP 0: get wallet snapshot.
            snapshot <- liftIO $ getWalletSnapshot walletPassive
            -- STEP 1: create witnesses.
            -- Since we already received inputs signatures with corresponding derived PKs,
            -- just form witnesses from them.
            let witnesses = V.fromList . NonEmpty.toList $ flip NonEmpty.map srcAddrsWithProofs $
                    \(_srcAddr, txSig, derivedPK) -> PkWitness derivedPK txSig

            -- STEP 2: make 'TxAux'.
            let txAux = TxAux tx witnesses

            -- STEP 3: Compute metadata
            let txId = hash tx
            -- Currently it's assumed that all source addresses belong to 'the same/ account,
            -- so we can just take the first source address to find our 'HdAccountId'.
            let (firstSrcAddress, _, _) = NonEmpty.head srcAddrsWithProofs
            firstSrcHdAddress <- withExceptT PaymentNoHdAddressForSrcAddress $ exceptT $
                lookupCardanoAddress snapshot firstSrcAddress
            let (HD.HdAddress (HD.HdAddressId srcAccountId _) _) = firstSrcHdAddress
            -- We use `getCreationTimestamp` provided by the `NodeStateAdaptor`
            -- to compute the createdAt timestamp for `TxMeta`.
            txMetaCreatedAt_ <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)

            -- STEP 4: Get available UTxO
            utxo <- withExceptT (PaymentNewTransactionError . NewTransactionUnknownAccount) $ exceptT $
                currentAvailableUtxo snapshot srcAccountId

            let inputs = NonEmpty.toList $ _txInputs tx
                maybeInputsWithCoins = map (collectInputCoins utxo) inputs
                inputsWithCoins = NonEmpty.fromList $ catMaybes maybeInputsWithCoins
            -- If 'tx' is valid (i.e. contains correct inputs), we already have
            -- all inputs with corresponding coins.
            let numberOfValidInputs = NonEmpty.length inputsWithCoins
                numberOfAllInputs = length maybeInputsWithCoins
            if numberOfValidInputs /= numberOfAllInputs then
                -- Something is wrong with inputs, this 'tx' should be rejected.
                ExceptT $ return $ Left $ PaymentNewTransactionError NewTransactionInvalidTxIn
            else do
                -- We have to calculate the sum of input coins.
                let spentInputCoins = paymentAmount (toaOut . snd <$> inputsWithCoins)

                -- STEP 5: form meta-data.
                partialMeta <- liftIO $ createNewMeta srcAccountId
                                                      txId
                                                      txMetaCreatedAt_
                                                      inputsWithCoins
                                                      (_txOutputs tx)
                                                      True
                                                      spentInputCoins
                -- STEP 6: our new pending tx.
                withExceptT PaymentNewPendingError $ ExceptT $
                    newPending aw srcAccountId txAux partialMeta
        case res of
            Left e -> do
                -- If the next retry would bring us to the end of our allowed retries,
                -- we fail with a proper error
                retriesLeft <- applyPolicy retryPolicy rs
                return $ case retriesLeft of
                    Nothing -> Left PaymentSubmissionMaxAttemptsReached
                    Just _  -> Left e
            Right meta ->
                return $ Right (tx, meta)
  where
    -- If utxo is valid, we definitely know that .
    collectInputCoins :: Utxo
                      -> TxIn
                      -> Maybe (TxIn, TxOutAux)
    collectInputCoins utxo txInput = case Map.lookup txInput utxo of
        Nothing       -> Nothing
        Just txOutput -> Just (txInput, txOutput)

-- | Creates a new 'TxAux' and corresponding 'TxMeta',
-- without submitting it to the network.
--
-- For testing purposes, if successful this additionally returns the utxo
-- that coin selection was run against.
newTransaction
    :: ActiveWallet
    -> PassPhrase
    -- ^ The spending password.
    -> CoinSelectionOptions
    -- ^ The options describing how to tune the coin selection.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> NonEmpty (Address, Coin)
    -- ^ The payees.
    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newTransaction aw@ActiveWallet{..} spendingPassword options accountId payees = do
    let pm = walletPassive ^. Internal.walletProtocolMagic
        nm = makeNetworkMagic pm
    tx <- newUnsignedTransaction aw options accountId payees
    case tx of
         Left e   -> return (Left e)
         Right (db, unsignedTx, _fees, availableUtxo) -> runExceptT $ do
             -- STEP 1: Perform the signing and forge the final TxAux.
             mbEsk <- liftIO $ Keystore.lookup
                        nm
                        (WalletIdHdRnd $ accountId ^. hdAccountIdParent)
                        (walletPassive ^. Internal.walletKeystore)

             -- STEP 2: Generate the change addresses needed.
             changeAddresses <- withExceptT NewTransactionErrorCreateAddressFailed $
                                  genChangeOuts (unsignedTxChange unsignedTx)
                                                accountId
                                                spendingPassword
                                                walletPassive

             let inputs      = unsignedTxInputs unsignedTx
                 outputs     = unsignedTxOutputs unsignedTx
                 signAddress = mkSigner nm spendingPassword mbEsk db
                 mkTx        = mkStdTx pm shuffleNE signAddress

             -- STEP 3: Creates the @signed@ transaction using data from the
             --         unsigned one.
             txAux <- withExceptT NewTransactionErrorSignTxFailed $ ExceptT $
                          mkTx inputs outputs changeAddresses

             -- STEP 4: Compute metadata
             let txId = hash . taTx $ txAux
             -- This is the sum of inputs coins.
             let spentInputCoins = paymentAmount (toaOut . snd <$> inputs)
             -- we use `getCreationTimestamp` provided by the `NodeStateAdaptor`
             -- to compute the createdAt timestamp for `TxMeta`
             txMetaCreatedAt_  <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)
             -- partially applied, because we don`t know here which outputs are ours
             partialMeta <- liftIO $ createNewMeta accountId txId txMetaCreatedAt_ inputs (_txOutputs . taTx $ txAux) True spentInputCoins
             return (txAux, partialMeta, availableUtxo)

-- | This is called when we create a new Pending Transaction.
-- This actually returns a function because we don`t know yet our outputs.
createNewMeta :: HdAccountId -> TxId -> Core.Timestamp -> NonEmpty (TxIn, TxOutAux) -> NonEmpty TxOut -> Bool -> Coin -> IO PartialTxMeta
createNewMeta hdId txId time inp out allInOurs spentInputsCoins = do
    -- this partially applied function indicates the lack of all TxMeta at this stage.
    return $ metaForNewTx time hdId txId inp out allInOurs spentInputsCoins

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
newtype Env = Env { getEnv :: System.Random.MWC.GenIO }

-- | \"Invalid\" 'MonadRandom' instance for 'PayMonad' which generates
-- randomness using the hash of 'NonEmpty (Address, Coin)' as fixed seed,
-- plus an internal counter used to shift the bits of such hash.
-- This ensures that the coin selection algorithm runs in a random environment
-- which is yet deterministically reproduceable by feeding the same set of
-- payees.
instance MonadRandom PayMonad where
    getRandomBytes len = do
        gen <- asks getEnv
        randomBytes <- liftIO (System.Random.MWC.asGenIO (flip System.Random.MWC.uniformVector len) gen)
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
             -> CoinSelectionOptions
             -- ^ The options describing how to tune the coin selection.
             -> HdAccountId
             -- ^ The source HD Account from where the payment should originate
             -> NonEmpty (Address, Coin)
             -- ^ The payees
             -> IO (Either EstimateFeesError Coin)
estimateFees activeWallet@ActiveWallet{..} options accountId payees = do
    res <- newUnsignedTransaction activeWallet options accountId payees
    case res of
         Left e  -> return . Left . EstFeesTxCreationFailed $ e
         Right (_db, _tx, fees, _originalUtxo) -> do
             -- sanity check of fees is done.
             return $ Right fees

-- | Calculate the fee as the difference between inputs and outputs. The
-- final 'sumOfOutputs' must be augmented by the change, which we have
-- available in the 'UnsignedTx' as a '[Coin]'.
--
-- NOTE(adn) In case of 'SenderPaysFee' is practice there might be a slightly
-- increase of the projected fee in the case we are forced to pick "yet another input"
-- to be able to pay the fee, which would, in turn, also increase the fee due to
-- the extra input being picked.
computeFeesOfUnsignedTx :: UnsignedTx -> Coin
computeFeesOfUnsignedTx unsginedTx =
    sumOfInputs unsginedTx
        `unsafeSubCoin`
                    (repeatedly Core.unsafeAddCoin (unsignedTxChange unsginedTx)
                                                   (sumOfOutputs unsginedTx))
    where
        -- Tribute to @edsko
        repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
        repeatedly = flip . foldl' . flip

        -- Unlike a block, a /single transaction/ cannot have inputs that sum to
        -- more than maxCoinVal
        sumOfInputs :: UnsignedTx -> Coin
        sumOfInputs tx =
            let inputs = fmap (toaOut . snd) . unsignedTxInputs $ tx
            in paymentAmount inputs

        sumOfOutputs :: UnsignedTx -> Coin
        sumOfOutputs tx =
            let outputs = map toaOut $ unsignedTxOutputs tx
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
    arbitrary = oneof
        [ SignTransactionMissingKey <$> arbitrary
        , SignTransactionErrorUnknownAddress <$> arbitrary
        , SignTransactionErrorNotOwned <$> arbitrary
        ]

mkSigner :: NetworkMagic
         -> PassPhrase
         -> Maybe EncryptedSecretKey
         -> DB
         -> Address
         -> Either SignTransactionError SafeSigner
mkSigner _ _ Nothing _ addr = Left (SignTransactionMissingKey addr)
mkSigner nm spendingPassword (Just esk) snapshot addr =
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
                res = Core.deriveLvl2KeyPair nm
                                             (Core.IsBootstrapEraAddr True)
                                             (ShouldCheckPassphrase False)
                                             spendingPassword
                                             esk
                                             accountIndex
                                             addressIndex
            -- eks address fix - we need to use the esk as returned
            -- from Core.deriveLvl2KeyPair rather than rely on the
            -- one from encrypted secret key delivered to mkSigner
            in case res of
                 Just (a, eskAddr) | a == addr ->
                     Right (SafeSigner eskAddr spendingPassword)
                 _otherwise              ->
                     Left (SignTransactionErrorNotOwned addr)

-- | An estimate of the Tx fees in Cardano based on a sensible number of defaults.
cardanoFee :: TxFeePolicy -> Int -> NonEmpty Coin -> Coin
cardanoFee (TxFeePolicyTxSizeLinear policy) inputs outputs =
    Core.mkCoin $
      estimateCardanoFee policy inputs (toList $ fmap Core.getCoin outputs)
cardanoFee TxFeePolicyUnknown{} _ _ =
    error "cardanoFee: unknown policy"

cardanoFeeSanity :: TxFeePolicy -> Coin -> Bool
cardanoFeeSanity (TxFeePolicyTxSizeLinear policy) fees =
    checkCardanoFeeSanity policy fees
cardanoFeeSanity TxFeePolicyUnknown{} _ =
    error "cardanoFeeSanity: unknown policy"

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
    let pm = walletPassive ^. Internal.walletProtocolMagic
    snapshot   <- liftIO $ getWalletSnapshot walletPassive
    _accExists <- withExceptT RedeemAdaUnknownAccountId $ exceptT $
                    lookupHdAccountId snapshot accId
    changeAddr <- withExceptT RedeemAdaErrorCreateAddressFailed $ ExceptT $ liftIO $
                    Kernel.createAddress
                      pw
                      (AccountIdHdRnd accId)
                      walletPassive
    (tx, meta) <- mkTx pm changeAddr
    withExceptT RedeemAdaNewForeignFailed $ ExceptT $ liftIO $
      newForeign
        w
        accId
        tx
        meta
    return (taTx tx, meta)
  where
    redeemAddr :: NetworkMagic -> Address
    redeemAddr nm = Core.makeRedeemAddress nm $ redeemToPublic rsk

    -- | Note: we use `getCreationTimestamp` provided by the `NodeStateAdaptor`
    --   to compute the createdAt timestamp for `TxMeta`
    mkTx :: ProtocolMagic -> Address -> ExceptT RedeemAdaError IO (TxAux, TxMeta)
    mkTx pm output = do
        let nm = makeNetworkMagic pm
        now  <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)
        utxo <- liftIO $
                  Node.withNodeState (walletPassive ^. walletNode) $ \_lock ->
                    Node.filterUtxo isOutput
        (inp@(TxInUtxo inHash inIx), coin) <-
          case utxo of
            [i]   -> return i
            []    -> throwError $ RedeemAdaNotAvailable    (redeemAddr nm)
            _:_:_ -> throwError $ RedeemAdaMultipleOutputs (redeemAddr nm)
        let out    = TxOutAux $ TxOut output coin
            txAux  = CTxp.makeRedemptionTx
                       pm
                       rsk
                       (inp :| [])
                       (out :| [])
            txMeta = TxMeta {
                _txMetaId         = hash (taTx txAux)
              , _txMetaAmount     = coin
              , _txMetaInputs     = (inHash, inIx, redeemAddr nm, coin) :| []
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
            let nm = makeNetworkMagic pm
            guard $ addr == (redeemAddr nm)
            return (inp, coin)

-- | Generates the list of change outputs from a list of change coins.
genChangeOuts :: MonadIO m
              => [Coin]
              -> HD.HdAccountId
              -> PassPhrase
              -> PassiveWallet
              -> ExceptT Kernel.CreateAddressError m [TxOutAux]
genChangeOuts changeCoins srcAccountId spendingPassword walletPassive =
    forM changeCoins $ \change -> do
        changeAddr <- genChangeAddr
        return TxOutAux {
            toaOut = TxOut
                { txOutAddress = changeAddr
                , txOutValue   = change
                }
        }
  where
    genChangeAddr :: MonadIO m
                  => ExceptT Kernel.CreateAddressError m Address
    genChangeAddr = ExceptT $ liftIO $
        Kernel.createAddress spendingPassword
                             (AccountIdHdRnd srcAccountId)
                             walletPassive

{-------------------------------------------------------------------------------
  Building transactions
-------------------------------------------------------------------------------}

-- | Our notion of @unsigned transaction@. Unfortunately we cannot reuse
-- directly the 'Tx' from @Core@ as that discards the information about
-- "ownership" of inputs, which is instead required when dealing with the
-- Core Txp.Util API.
data UnsignedTx = UnsignedTx {
      unsignedTxInputs     :: !(NonEmpty (Core.TxIn, Core.TxOutAux))
    , unsignedTxOutputs    :: !(NonEmpty Core.TxOutAux)
    , unsignedTxAttributes :: !Core.TxAttributes
    , unsignedTxChange     :: ![Core.Coin]
}

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => ProtocolMagic
        -> (forall a. NonEmpty a -> m (NonEmpty a))
        -- ^ Shuffle function
        -> (Core.Address -> Either e SafeSigner)
        -- ^ Signer for each input of the transaction
        -> NonEmpty (Core.TxIn, Core.TxOutAux)
        -- ^ Selected inputs
        -> NonEmpty Core.TxOutAux
        -- ^ Selected outputs
        -> [Core.TxOutAux]
        -- ^ Change outputs
        -> m (Either e Core.TxAux)
mkStdTx pm shuffle hdwSigners inps outs change = do
    allOuts <- shuffle $ foldl' (flip NonEmpty.cons) outs change
    return $ CTxp.makeMPubKeyTxAddrs pm hdwSigners (fmap repack inps) allOuts
    where
         -- | Repacks a utxo-derived tuple into a format suitable for
         -- 'TxOwnedInputs'.
        repack :: (Core.TxIn, Core.TxOutAux) -> (Core.TxOut, Core.TxIn)
        repack (txIn, aux) = (Core.toaOut aux, txIn)
