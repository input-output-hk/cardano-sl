{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}


module RandomStateWalk (randomStateWalkTest)
where

import           Universum hiding (init, throwM, uncons)

import           Control.Lens (at, each, filtered, uses, (%=), (+=), (.=),
                     (<>=), (?=))
import           Control.Monad.Catch (throwM)
import           Data.Aeson (toJSON)
import           Data.Aeson.Diff (diff)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.List (isInfixOf, nub, uncons, (!!), (\\))
import           Data.Map (fromList)
import           Data.Traversable (for)
import           Formatting (sformat)
import           Servant.Client (GenResponse (..))
import           Test.Hspec (describe, expectationFailure, hspec, shouldBe,
                     shouldContain)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, elements, frequency,
                     quickCheck, suchThat, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadic, monadicIO, pick,
                     pre, run, stop)
import           Test.QuickCheck.Property (Property, ioProperty, rejected)

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.Client (ClientError (..), ServantError (..),
                     WalletClient (..), WalletError (..), getAccounts,
                     getAddressIndex, getTransactionIndex, getWallets,
                     hoistClient)

import           Pos.Core (addressF, getCoin, mkCoin, unsafeAddCoin,
                     unsafeSubCoin)

import           Error (WalletTestError (..))
import           Types
import           Util

randomStateWalkTest :: WalletClient IO -> IO ()
randomStateWalkTest walletClient = do
    printT "Starting the integration testing for wallet."
    walletState <- initialWalletState walletClient
    printT $ "Initial wallet state: " <> show walletState
    void $ runActionCheck walletClient walletState actionDistribution

newtype RefT s m a
    = RefT
    { unRefT :: ReaderT (IORef s) m a
    } deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans, Alternative, MonadCatch
    , MonadThrow, MonadPlus
    )

instance MonadIO m => MonadState s (RefT s m) where
    get = RefT ask >>= readIORef
    put x = RefT ask >>= (`writeIORef` x)

runRefT :: MonadIO m => RefT s m a -> s -> m (a, s)
runRefT act i = do
    init <- newIORef i
    a <- runReaderT (unRefT act) init
    s <- readIORef init
    pure (a, s)

execRefT :: MonadIO m => RefT s m a -> s -> m s
execRefT x = fmap snd . runRefT x

monadicRefIO :: s -> PropertyM (RefT s IO) () -> Property
monadicRefIO ws = monadic $ \reft'io'prop ->
    let ioProp = fmap fst (runRefT reft'io'prop ws)
     in ioProperty ioProp

instance MonadIO m => MonadState s (PropertyM (RefT s m)) where
    get = lift $ (get :: RefT s m s)
    put x = lift $ (put x :: RefT s m ())

instance (MonadIO m, MonadThrow m) => MonadThrow (PropertyM (RefT s m)) where
    throwM = lift . throwM

-- | This function uses the given 'ActionProbabilities' to generate a list
-- of actions to test against the API. It runs the actions through,
-- eventually returning the final state.
runActionCheck
    :: (WalletTestMode m,
        HasCallStack
       )
    => WalletClient IO
    -> WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletClient walletState actionProb = do
    let randomTests = monadicIO $ do
            actions <-  chooseActions 50 actionProb
            log $ "Test will run these actions: " <> show (toList actions)
            let client' = hoistClient lift walletClient
            liftIO $ ( quickCheck $ monadicRefIO walletState
                       (traverse_ (runAction client') actions)
                     )
                `Universum.catch`
                \case
                    LocalWalletDiffers a b ->
                        liftIO $ a `shouldBe` b
                    LocalWalletsDiffers as bs ->
                        liftIO $ sort as `shouldBe` sort bs
                    LocalAccountDiffers a b ->
                        liftIO $ a `shouldBe` b
                    LocalAccountsDiffers as bs ->
                        liftIO $ sort as `shouldBe` sort bs
                    LocalAddressesDiffer as bs ->
                        liftIO $ sort as `shouldBe` sort bs
                    LocalAddressDiffer a b ->
                        liftIO $ a `shouldBe` b
                    LocalTransactionsDiffer as bs ->
                        liftIO $ sort as `shouldBe` sort bs
                    LocalTransactionMissing txn txns ->
                        liftIO $ txns `shouldContain` [txn]
                    err ->
                        liftIO $ expectationFailure $ show err

    void $ liftIO $ hspec $ describe "Random Walk Test" $
                prop ("50 API calls") $ withMaxSuccess 1 $ randomTests

    _ <- execRefT report walletState
    pure walletState
  where
    report = do
        acts <- use actionsNum
        succs <- use successActions
        log $ "Successfully run " <> show (length succs) <> " out of " <> show acts <> " actions"
        log $ "Successful actions counts: " <> show (map (\a -> (a!!0, length a)) $ group $ sort succs)
        log $ "Skipped actions: " <> show ([minBound..maxBound] \\ nub succs)


freshPassword :: (MonadState WalletState (PropertyM m), MonadIO m) => PropertyM m SpendingPassword
freshPassword = do
    passwords <- gets (toList . view walletsPass)
    pick $ arbitrary `suchThat` (`notElem` passwords)

-- | Here we run the actions.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
-- NOTE: ordNub is available in latest universum, but current universum-0.9 doesn't have it.
{-# ANN module ("HLint: ignore Use ordNub" :: Text) #-}
runAction
    :: (WalletTestMode m,
        MonadState WalletState (PropertyM m),
        MonadThrow (PropertyM m)
        )
    => WalletClient m
    -> Action
    -> PropertyM m ()
runAction wc action = do

    previousWalletState <- get

    log $ "Action Selected: " <> show action
    actionsNum += 1
    acts <- use actionsNum
    succs <- length <$> use successActions
    log $ "Actions:\t" <> show acts <> "\t\tSuccesses:\t" <> show succs

    case action of
        PostWallet -> do
            newPassword <- freshPassword
            newWall     <- pick $ generateNewWallet newPassword
            log $ "Request: " <> ppShowT newWall
            eresult      <- run $ postWallet wc newWall

            case eresult of
                Right WalletResponse { wrData = result } -> do
                    checkInvariant
                        (walBalance result == minBound)
                        (WalletBalanceNotZero result)

                    walletAccounts <- respToRes . getAccounts wc $ walId result

                    log $ "New wallet ID: " <> ppShowT (walId result)
                    log $ "New accounts: " <> ppShowT walletAccounts

                    -- Modify wallet state accordingly.
                    wallets    <>= [result]
                    walletsPass . at (walId result) ?= newPassword
                    accounts   <>= walletAccounts
                    addresses  <>= concatMap accAddresses walletAccounts

                Left (ClientHttpError (FailureResponse (Response {..})))
                    | "mnemonics" `isInfixOf` show responseBody -> do
                        log "Mnemonic was already taken!"
                        -- we need to record that the password is in use
                        -- somehow
                        dummyWallet <- pick arbitrary
                        walletsPass . at dummyWallet ?= newPassword
                Left err ->
                    liftIO $ throwM err
          where
            generateNewWallet spendPass =
                NewWallet
                    <$> arbitrary
                    <*> pure (Just spendPass)
                    <*> arbitrary
                    <*> pure "Wallet"
                    <*> pure CreateWallet

        GetWallets -> do
            result  <-  respToRes $ getWallets wc
            localWallets <- use wallets
            checkInvariant
                (length localWallets == length result)
                (LocalWalletsDiffers result localWallets)
            wallets .= result

            -- No modification required.

        GetWallet -> do
            -- We choose from the existing wallets.
            wal <-  pickRandomElement =<< use wallets

            log $ "Requesting wallet: " <> show (walId wal)
            result  <-  respToRes $ getWallet wc (walId wal)

            checkInvariant
                (walId result == walId wal)
                (LocalWalletDiffers result wal)

        DeleteWallet -> do
            -- We are using non genesis wallets because we don't want to remove wallet with money during tests
            localWallets <- use nonGenesisWallets

            -- We choose from the existing wallets.
            wallet  <-  pickRandomElement localWallets

            log $ "Deleting: " <> show (walId wallet)
            -- If we don't have any http client errors, the delete was a success.
            _       <-  run $ either throwM pure =<< deleteWallet wc (walId wallet)

            -- Just in case, let's check if it's still there.
            result  <-  respToRes $ getWallets wc

            checkInvariant
                (walId wallet `notElem` map walId result)
                (LocalWalletsDiffers result localWallets)

            walletAccounts <- uses accounts (filter (\acc -> accWalletId acc /= walId wallet))

            -- Modify wallet state accordingly.
            wallets  %= filter (\w -> walId wallet /= walId w)
            accounts .= walletAccounts
            addresses %= filter (`elem` concatMap accAddresses walletAccounts)
            walletsPass . at (walId wallet) .= Nothing

        UpdateWallet -> do

            localWallets <- use nonGenesisWallets

            -- We choose from the existing wallets.
            wallet  <-  pickRandomElement localWallets

            let walletId = walId wallet

            let newWallet =
                    WalletUpdate
                        { uwalAssuranceLevel = NormalAssurance
                        , uwalName           = "Wallet name " <> (show walletId)
                        }

            log $ "Updating wallet: " <> show walletId <> ", " <> ppShowT newWallet
            result  <-  respToRes $ updateWallet wc walletId newWallet
            log $ "Old   : " <> ppShowT wallet
            log $ "Result: " <> ppShowT result
            -- Modify wallet state accordingly.
            -- TODO(matt.parsons): Uncomment this line, when we actually
            -- implement wallet update in CSL-2450
            -- wallets . each . filtered (== wallet) .= result

        UpdateWalletPass -> do

            localWallets <- use nonGenesisWallets

            -- We choose from the existing wallets.
            wallet  <-  pickRandomElement localWallets

            let walletId = walId wallet

            -- Get the old wallet password.
            oldWalletPass <- use (walletsPass . at (walId wallet))

            walletPass  <- maybe (pure mempty :: PropertyM m SpendingPassword) pure oldWalletPass
            newPassword <- freshPassword

            -- Create a password update.
            let newPasswordUpdate =
                    PasswordUpdate
                        { pwdOld = walletPass
                        , pwdNew = newPassword
                        }

            log $ "Wallet ID: " <> show walletId
            log $ "Updating wallet password: " <> ppShowT newPasswordUpdate
            result  <-  respToRes $ updateWalletPassword wc walletId newPasswordUpdate
            checkInvariant
                (walId result == walId wallet)
                (LocalWalletDiffers result wallet)

            -- Modify wallet state accordingly.
            wallets %= map (\wal -> if walId wal == walId wallet then result else wal)
            walletsPass . at (walId result) ?= newPassword

-- Accounts
        PostAccount -> do

            localWallets <- use wallets

            wallet  <- pickRandomElement localWallets
            mpass   <- use (walletsPass . at (walId wallet))
            newAcc  <-  pick $ generateNewAccount mpass
            log $ "Posting account: " <> show (walId  wallet) <> ", " <> ppShowT newAcc
            result  <-  respToRes $ postAccount wc (walId wallet) newAcc

            checkInvariant
                (accAmount result == minBound)
                (AccountBalanceNotZero result)

            log $ "New account: " <> ppShowT result

            -- Modify wallet state accordingly.
            accounts   <>= [result]
            addresses  <>= accAddresses result
          where
            generateNewAccount mpass = do
                i <- arbitrary
                name <- replicateM i (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
                pure $ NewAccount mpass (fromString name)

        GetAccounts   -> do
            -- We choose from the existing wallets AND existing accounts.
            wallet  <-  pickRandomElement =<< use wallets
            let walletId = walId wallet

            -- We get all the accounts.
            log $ "Getting accounts for walletID: " <> show walletId
            log $ "Wallet info: " <> ppShowT wallet
            result  <-  respToRes $ getAccounts wc walletId

            accts <- uses accounts (filter ((walletId ==) . accWalletId))
            checkInvariant
                (length result == length accts)
                (LocalAccountsDiffers accts result)
            pure ()

        GetAccount    -> do
            -- We choose from the existing wallets AND existing accounts.
            account <- pickRandomElement =<< use accounts
            let walletId = accWalletId account
            walletIdIsNotGenesis walletId

            log $ "Getting account: " <> show walletId <> ", " <> show (accIndex account)
            result  <-  respToRes $ getAccount wc walletId (accIndex account)

            -- posting transactions changes the account, so we really want
            -- to check that nothing *but* the amount differs.
            let eqOn :: Eq b => (Account -> b) -> Bool
                eqOn f = f account == f result
            checkInvariant
                (and [eqOn accIndex, eqOn (length . accAddresses), eqOn accName, eqOn accWalletId])
                (LocalAccountDiffers result account)

        DeleteAccount -> do
            localAccounts <- use accounts

            -- We choose from the existing wallets AND existing accounts.
            account <-  pickRandomElement localAccounts
            let walletId = accWalletId account
                accIdx = accIndex account
                withoutAccount = filter (not . chosenAccount) localAccounts
                chosenAccount acct =
                    accWalletId acct == walletId
                    && accIndex acct ==  accIdx

            -- We don't want to delete accounts in genesis walletsG
            walletIdIsNotGenesis walletId
            log $ "Deleting account, walletID: " <> show walletId <> ", accIndex: " <> show accIdx
            -- If we don't have any http client errors, the delete was a success.
            _ <- run $ either throwM pure =<< deleteAccount wc walletId accIdx

            -- Just in case, let's check if it's still there.
            result  <-  respToRes $ getAccounts wc walletId

            checkInvariant
                (account `notElem` result)
                (LocalAccountsDiffers result withoutAccount)

            -- Modify wallet state accordingly.
            accounts  .= withoutAccount
            addresses %= filter (`notElem` accAddresses account)

        UpdateAccount -> do
            localAccounts <- use accounts

            -- We choose from the existing wallets.
            account <-  pickRandomElement localAccounts

            let walletId  = accWalletId account
            let accountId = accIndex account

            let newAccount =
                    AccountUpdate
                        { uaccName = "Account name " <> (show accountId)
                        }

            -- We don't want to update genesis wallet account (althought it should be safe)
            walletIdIsNotGenesis walletId

            log $ mconcat ["Updating account: ", show walletId, ", ", show accountId, ", ", ppShowT newAccount]
            result  <-  respToRes $ updateAccount wc walletId accountId newAccount

            -- Modify wallet state accordingly.
            accounts . each . filtered (== account) .= result

-- Addresses
        PostAddress -> do
            -- The precondition is that we must have accounts.
            -- If we have accounts, that presupposes that we have wallets,
            -- which is the other thing we need here.
            localAccounts <- use accounts

            -- We choose from the existing wallets AND existing accounts.
            account <-  pickRandomElement localAccounts
            let walletId = accWalletId account
            walletPass <- use (walletsPass . at walletId)

            let newAddress = NewAddress walletPass (accIndex account) walletId

            -- We don't want to create a new address in genesis wallet (although it should be safe)
            walletIdIsNotGenesis walletId

            log $ "Posting address: " <> ppShowT newAddress
            result  <-  respToRes $ postAddress wc newAddress

            -- Modify wallet state accordingly.
            addresses  <>= [result]
            accounts . traverse . filtered (== account) %= \acct ->
                acct { accAddresses = accAddresses acct <> [result] }

        GetAddresses   -> do
            -- We set the state to be the result of calling the endpoint.
            -- Attempting to keep the addresses in sync with the server
            -- introduces a lot of complexity, and mostly catches bugs in
            -- the local state management.
            result <- respToRes $ getAddressIndex wc
            addresses .= result

        GetAddress -> do
            -- We choose one address.
            (V1 address) <-  fmap addrId . pickRandomElement =<< use addresses

            let textAddress = sformat addressF address

            log $ "Requesting: " <> textAddress

            -- If the address exists, it is valid.
            void . respToRes $ getAddress wc textAddress

        -- Transactions
        PostTransaction -> do
            localAccounts  <- use accounts

            -- Some min amount of money so we can send a transaction?
            -- https://github.com/input-output-hk/cardano-sl/blob/develop/lib/configuration.yaml#L228
            let minCoinForTxs = V1 . mkCoin $ 200000
            let localAccsNotLocked = filter ((/= lockedWallet) . accWalletId) localAccounts
            let localAccsWithMoney = filter ((> minCoinForTxs) . accAmount) localAccsNotLocked

            -- From which source to pay.
            accountSource <- pickRandomElement localAccsWithMoney
            accountDestination <- pickRandomElement
                (filter (not . accountsHaveSameId accountSource) localAccounts)
            log $ "From account: " <> show (accIndex accountSource)  <> "\t\t" <> show (accWalletId accountSource)
            log $ "To account  : " <> show (accIndex accountDestination) <> "\t\t" <> show (accWalletId accountDestination)

            let accountSourceMoney = accAmount accountSource
                reasonableFee = 100

            -- We should probably have a sensible minimum value.
            moneyAmount <- fmap mkCoin . pick
                $ choose
                    ( 10
                    , getCoin (unV1 accountSourceMoney) - reasonableFee
                    )
            -- moneyAmount <- pick arbitrary

            let paymentSource =
                    PaymentSource
                        { psWalletId     = accWalletId accountSource
                        , psAccountIndex = accIndex    accountSource
                        }

            addressDestination <- pickRandomElement $ accAddresses accountDestination

            let paymentDestinations =
                    PaymentDistribution
                        { pdAddress = addrId addressDestination
                        , pdAmount  = V1 moneyAmount
                        } :| []

            walletPass <- use (walletsPass . at (accWalletId accountSource))
            let newPayment = Payment
                                 paymentSource
                                 paymentDestinations
                                 mzero
                                 walletPass

            -- Check the transaction fees.
            log $ "getTransactionFee: " <> ppShowT newPayment
            etxFees  <-  run $ fmap wrData <$> getTransactionFee wc newPayment

            txFees <- case etxFees of
                Right a -> pure a
                Left (ClientWalletError (NotEnoughMoney _)) -> do
                        log "Not enough money to do the transaction."
                        (pure $ EstimatedFees (V1 . mkCoin $ 0))
                Left err -> throwM err


            checkInvariant
                (feeEstimatedAmount txFees > minBound)
                (InvalidTransactionFee txFees)

            -- Check the transaction.
            log $ "postTransaction: " <> ppShowT newPayment
            addressesBeforeTransaction <- respToRes $ getAddressIndex wc
            newTx  <-  respToRes $ postTransaction wc newPayment
            addressesAfterTransaction <- respToRes $ getAddressIndex wc

            let sumCoins f =
                    sum . map (getCoin . unV1 . pdAmount) . toList $ f newTx
                inputSum = sumCoins txInputs
                outputSum = sumCoins txOutputs

            -- Transaction shouldn't produce any money
            checkInvariant
                (inputSum >= outputSum)
                (InvalidTransactionState newTx)

            let actualFees = V1 . mkCoin $ inputSum - outputSum

            -- Estimated fees should correspond to actual fees
            checkInvariant
                (feeEstimatedAmount txFees == actualFees)
                (InvalidTransactionFee txFees)

            let changeAddress = toList (txOutputs newTx) \\ toList paymentDestinations
                -- NOTE: instead of this manual conversion we could filter WalletAddress from getAddressIndex
                pdToChangeAddress PaymentDistribution{..} = WalletAddress pdAddress True True (V1 AddressIsOurs)
                realChangeAddressId = map addrId addressesAfterTransaction \\ map addrId addressesBeforeTransaction
                changeWalletAddresses = filter ((`elem` realChangeAddressId) . addrId) addressesAfterTransaction

            -- We expect at most one extra PaymentDestination which should be a change address. Also at most one address should be added after transaction - which should be the same change address
            -- All change addresses should set up a flag addrChangeAddress
            checkInvariant
                ( length changeAddress <= 1
                  && map pdAddress changeAddress == realChangeAddressId
                  && all addrChangeAddress changeWalletAddresses
                )
                (UnexpectedChangeAddress changeWalletAddresses)

            _accountSourceAfter <- respToRes $
                getAccount wc
                    (accWalletId accountSource)
                    (accIndex accountSource)

            _accountDestinationAfter <- respToRes $
                getAccount wc
                    (accWalletId accountDestination)
                    (accIndex accountDestination)

            let _expectedNewBalance =
                    V1 $
                        (unV1 (accAmount accountSource) `unsafeSubCoin` moneyAmount)
                        `unsafeSubCoin` unV1 actualFees

            -- Check whether the source account decrease by expected amount after tx
            --checkInvariant
            --    (accAmount accountSourceAfter == expectedNewBalance)
            --    (UnexpectedAccountBalance
            --        "Account source should decrease"
            --        (accAmount accountSourceAfter)
            --        expectedNewBalance
            --    )


            let _expectedDestinationBalance =
                    V1 (unV1 (accAmount accountDestination)
                        `unsafeAddCoin` moneyAmount)

            ---- Check whether the destination account increased by expected amount after tx
            --checkInvariant
            --    (accAmount accountDestinationAfter == expectedDestinationBalance)
            --    (UnexpectedAccountBalance
            --        "Account destination should increase"
            --        (accAmount accountDestination)
            --        expectedDestinationBalance
            --    )

            -- Modify wallet state accordingly.
            transactions  <>= [(accountSource, newTx)]
            addresses     <>= map pdToChangeAddress changeAddress

        GetTransaction  -> do
            txs <- use transactions

            -- We choose from the existing transactions.
            accTransaction  <- pickRandomElement txs

            let txsAccount :: Account
                txsAccount = accTransaction ^. _1

            let transaction :: Transaction
                transaction = accTransaction ^. _2

            let walletId :: WalletId
                walletId = accWalletId txsAccount

            let accountIndex :: AccountIndex
                accountIndex = accIndex txsAccount

            log $ "getTransactionIndex: " <> show walletId <> ", " <> show accountIndex
            result  <-  respToRes $ getTransactionIndex
                                        wc
                                        (Just walletId)
                                        (Just accountIndex)
                                        Nothing

            -- First check we have results
            checkInvariant
                (not (null result))
                (LocalTransactionsDiffer result (map snd txs))

            -- Then check if the transaction exists in the history
            checkInvariant
                (txId transaction `elem` map txId result)
                (LocalTransactionMissing transaction result)

        NoOp  -> pure ()

    lastAction .= action

    -- Let's print it out to JSON
    walletState <- get

    log "=================================================================="
    log . decodeUtf8 . encodePretty $ diff (toJSON previousWalletState) (toJSON walletState)
    log "=================================================================="

    -- increment successful actions
    log "Success!"
    successActions <>= [action]


-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


-- | Generate action randomly, depending on the action distribution.
-- chooseActionGen
--     :: ActionProbabilities
--     -> Gen Action
-- chooseActionGen =
--     frequency . map (\(a, p) -> (getWeight p, pure a)) . toList


-- | Generate action from the generator.
-- chooseAction
--     :: (WalletTestMode m)
--     => ActionProbabilities
--     -> m Action
-- chooseAction = pick . chooseActionGen

-- | Generate a random sequence of actions with the given size.
chooseActions
    :: (WalletTestMode m)
    => Word
    -> ActionProbabilities
    -> PropertyM m (NonEmpty Action)
chooseActions n probs = pick $ do
    let gens = map (\(a, p) -> (getWeight p, pure a)) (toList probs)
    a <- frequency gens
    as <- replicateM (fromIntegral n - 1) (frequency gens)
    pure (a :| as)

-- | We are not interested in the @WalletResponse@ for now.
respToRes
    :: forall m a. (MonadThrow m)
    => m (Either ClientError (WalletResponse a))
    -> PropertyM m a
respToRes resp = do
    result <- lift resp
    run $ either throwM (pure . wrData) result


-- | Pick a random element using @IO@.
pickRandomElement :: (Show a, MonadIO m) => [a] -> PropertyM m a
pickRandomElement [] = stop rejected
pickRandomElement xs = pick . elements $ xs

-- | A util function for checking the validity of invariants.
checkInvariant
    :: forall m. (MonadIO m)
    => Bool
    -> WalletTestError
    -> PropertyM m ()
checkInvariant True  _             = pure ()
checkInvariant False walletTestErr = liftIO $ throwM walletTestErr

actionDistribution :: ActionProbabilities
actionDistribution =
        (PostWallet, Weight 2)
            :| (PostTransaction, Weight 5)
            : fmap (, Weight 1) [minBound .. maxBound]

initialWalletState :: WalletClient IO -> IO WalletState
initialWalletState wc = do
    -- We will have single genesis wallet in intial state that was imported from launching script
    _wallets <- fromResp $ getWallets wc
    _accounts <- concat <$> for _wallets (fromResp . getAccounts wc . walId)
    -- Lets set all wallet passwords for initial wallets (genesis) to default (emptyPassphrase)
    let _lastAction       = NoOp
        _walletsPass      = fromList $ map ((, V1 mempty) . walId) _wallets
        _addresses        = concatMap accAddresses _accounts
        -- TODO(akegalj): I am not sure does importing a genesis wallet (which we do prior launching integration tests) creates a transaction
        -- If it does, we should add this transaction to the list
        _transactions     = mempty
        _actionsNum       = 0
        _successActions   = mempty
    return WalletState {..}
  where
    fromResp = (either throwM (pure . wrData) =<<)

walletIdIsNotGenesis
    :: (Monad m,
        MonadState WalletState (PropertyM m)
       )
    => WalletId -> PropertyM m ()
walletIdIsNotGenesis walletId = do
    mwallet <- (fmap fst . uncons) . filter ((walletId ==) . walId) <$> use wallets
    whenJust mwallet $ \wal ->
        pre (walName wal /= "Genesis wallet")
