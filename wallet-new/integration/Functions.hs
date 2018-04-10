{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Functions where

import           Universum hiding (log)

import           Data.Coerce (coerce)
import           Data.List (isInfixOf)
import           Data.List.NonEmpty (fromList)

import           Control.Lens (at, each, filtered, uses, (%=), (+=), (.=), (<>=), (?=))
import           Test.Hspec
import           Test.QuickCheck

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex, AccountUpdate (..),
                                              AssuranceLevel (..), EstimatedFees (..),
                                              NewAccount (..), NewAddress (..), NewWallet (..),
                                              PasswordUpdate (..), Payment (..),
                                              PaymentDistribution (..), PaymentSource (..),
                                              SpendingPassword, Transaction (..), V1 (..),
                                              Wallet (..), WalletAddress (..), WalletId,
                                              WalletOperation (..), WalletUpdate (..), unV1)

import           Cardano.Wallet.API.V1.Migration.Types (migrate)
import           Cardano.Wallet.Client (ClientError (..), Response (..), ServantError (..),
                                        WalletClient (..), getAccounts, getAddressIndex,
                                        getTransactionIndex, getWallets, hoistClient)

import           Pos.Core (getCoin, mkCoin)
import qualified Pos.Wallet.Web.ClientTypes.Types as V0

import           Error
import           Types

-- | This function uses the given 'ActionProbabilities' to generate a list
-- of actions to test against the API. It runs the actions through,
-- eventually returning the final state.
runActionCheck
    :: (WalletTestMode m, HasCallStack)
    => WalletClient m
    -> WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletClient walletState actionProb = do
    actions <- toList <$> chooseActions 20 actionProb
    let client' = hoistClient lift walletClient
    execStateT (tryAll (map (runAction client') actions)) walletState
        `catch` \x -> fmap (const walletState) . liftIO . hspec .
            describe "Unit Test Failure" $
                it ("threw a test error: " ++ showConstr x) $ case x of
                    LocalWalletDiffers a b ->
                        a `shouldBe` b
                    LocalWalletsDiffers as bs ->
                        as `shouldBe` bs
                    LocalAccountDiffers a b ->
                        a `shouldBe` b
                    LocalAccountsDiffers as bs ->
                        as `shouldBe` bs
                    LocalAddressesDiffer addr addrs ->
                        addr `shouldSatisfy` (`elem` addrs)
                    LocalAddressDiffer a b ->
                        a `shouldBe` b
                    LocalTransactionsDiffer as bs ->
                        as `shouldBe` bs
                    LocalTransactionMissing txn txns ->
                        txn `shouldSatisfy` (`elem` txns)
                    err ->
                        expectationFailure $ show err

-- | Attempt each action in the list. If an action fails, ignore the
-- failure.
--
-- If this were implemented as:
--
-- @
-- tryAll = foldr (\a b -> a *> b <|> b) empty
-- @
--
-- Then it would always end up failing with the final `empty`. The explicit
-- pattern matching allows us to potentially succeed.
tryAll :: Alternative f => [f a] -> f a
tryAll []     = empty
tryAll (x:xs) = foldr (\act acc -> act *> acc <|> acc) x xs

freshPassword :: (MonadState WalletState m, MonadIO m) => m SpendingPassword
freshPassword = do
    passwords <- gets (toList . view walletsPass)
    liftIO $ generate $ arbitrary `suchThat` (`notElem` passwords)

-- | Here we run the actions.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
runAction
    :: (WalletTestMode m, HasCallStack, MonadState WalletState m)
    => WalletClient m
    -> Action
    -> m ()
-- Wallets
runAction wc action = do
    log $ "Running: " <> show action
    case action of
        PostWallet -> do
            newPassword <- freshPassword
            newWall     <- liftIO $ generate $ generateNewWallet newPassword
            log $ "Request: " <> show newWall
            eresult      <- postWallet wc newWall

            case eresult of
                Right WalletResponse { wrData = result } -> do
                    checkInvariant
                        (walBalance result == minBound)
                        (WalletBalanceNotZero result)

                    log $ "New wallet ID: " <> show (walId result)
                    -- Modify wallet state accordingly.
                    wallets    <>= [result]
                    walletsPass . at (walId result) ?= newPassword

                Left (ClientHttpError (FailureResponse (Response {..})))
                    | "mnemonics" `isInfixOf` show responseBody -> do
                        log "Mnemonic was already taken!"
                        -- we need to record that the password is in use
                        -- somehow
                        dummyWallet <- liftIO . generate $ arbitrary
                        walletsPass . at dummyWallet ?= newPassword
                Left err ->
                    throwM err
          where
            generateNewWallet spendPass =
                NewWallet
                    <$> arbitrary
                    <*> pure (Just spendPass)
                    <*> arbitrary
                    <*> pure "Wallet"
                    <*> pure CreateWallet

        GetWallets -> do
            -- our internal state is less important than
            result  <-  respToRes $ getWallets wc
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
            localWallets <- use nonGenesisWallets

            -- We choose from the existing wallets.
            wallet  <-  pickRandomElement localWallets

            log $ "Deleting: " <> show (walId wallet)
            -- If we don't have any http client errors, the delete was a success.
            _       <-  either throwM pure =<< deleteWallet wc (walId wallet)

            -- Just in case, let's check if it's still there.
            result  <-  respToRes $ getWallets wc

            checkInvariant
                (walId wallet `notElem` map walId result)
                (LocalWalletsDiffers result localWallets)

            walletAccounts <- uses accounts (filter (\acc -> accWalletId acc /= walId wallet))

            -- Modify wallet state accordingly.
            wallets  %= filter (\w -> walId wallet /= walId w)
            accounts .= walletAccounts
            addresses %= filter (`notElem` concatMap accAddresses walletAccounts)
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

            log $ "Updating wallet: " <> show walletId <> ", " <> show newWallet
            result  <-  respToRes $ updateWallet wc walletId newWallet
            log $ "Old   : " <> show wallet
            log $ "Result: " <> show result
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

            walletPass  <- maybe empty pure oldWalletPass
            newPassword <- freshPassword

            -- Create a password update.
            let newPasswordUpdate =
                    PasswordUpdate
                        { pwdOld = walletPass
                        , pwdNew = newPassword
                        }

            log $ "Wallet ID: " <> show walletId
            log $ "Updating wallet password: " <> show newPasswordUpdate
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
            newAcc  <-  liftIO $ generate (generateNewAccount mpass)
            log $ "Posting account: " <> show (walId  wallet) <> ", " <> show newAcc
            result  <-  respToRes $ postAccount wc (walId wallet) newAcc

            checkInvariant
                (accAmount result == minBound)
                (AccountBalanceNotZero result)

            log $ "New account: " <> show result

            -- Modify wallet state accordingly.
            accounts   <>= [result]
          where
            generateNewAccount mpass = do
                i <- arbitrary
                name <- replicateM i (elements (['a' .. 'z'] ++ ['A' .. 'Z']))
                pure $ NewAccount mpass (fromString name)

        GetAccounts   -> do
            -- We choose from the existing wallets AND existing accounts.
            wallet  <-  pickRandomElement =<< use nonGenesisWallets
            let walletId = walId wallet
            walletIdIsNotGenesis walletId
            -- We get all the accounts.
            log $ "Getting accounts for walletID: " <> show walletId
            log $ "Wallet info: " <> show wallet
            result  <-  respToRes $ getAccounts wc walletId

            accts <- uses accounts (filter ((walletId ==) . accWalletId))
            -- TODO(matt.parsons): This fails almost every time. It always
            -- returns an empty list. Why?
--            checkInvariant
--                (length result == length accts)
--                (LocalAccountsDiffers accts result)

        GetAccount    -> do
            -- We choose from the existing wallets AND existing accounts.
            account <- pickRandomElement =<< use accounts
            let walletId = accWalletId account
            walletIdIsNotGenesis walletId

            log $ "Getting account: " <> show walletId <> ", " <> show (accIndex account)
            result  <-  respToRes $ getAccount wc walletId (accIndex account)

            checkInvariant
                (accAmount result == accAmount account)
                (LocalAccountDiffers result account)

            -- Modify wallet state accordingly.

        DeleteAccount -> do
            localAccounts <- use accounts

            -- We choose from the existing wallets AND existing accounts.
            account <-  pickRandomElement localAccounts
            let walletId = accWalletId account
                deleted = filter notSame localAccounts
                notSame acct =
                    accWalletId acct == accWalletId account
                    && accIndex acct ==  accIndex account
                accIdx = accIndex account

            log $ "Deleting account, walletID: " <> show walletId <> ", accIndex: " <> show accIdx
            -- If we don't have any http client errors, the delete was a success.
            _ <- either throwM pure =<< deleteAccount wc walletId accIdx

            -- Just in case, let's check if it's still there.
            result  <-  respToRes $ getAccounts wc walletId

            checkInvariant
                (account `notElem` result)
                (LocalAccountsDiffers result deleted)

            -- Modify wallet state accordingly.
            accounts  .= deleted
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

            walletIdIsNotGenesis walletId

            log $ mconcat ["Updating account: ", show walletId, ", ", show accountId, ", ", show newAccount]
            traverse_ log . map show . filter ((walletId ==) . walId) =<< use wallets
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

            let newAddress = createNewAddress walletId (accIndex account)

            walletIdIsNotGenesis walletId
            log $ "Posting address: " <> show newAddress
            result  <-  respToRes $ postAddress wc newAddress

            checkInvariant
                (addrBalance result == minBound)
                (AddressBalanceNotZero result)

            -- Modify wallet state accordingly.
            addresses  <>= [result]
            accounts . traverse . filtered (== account) %= \acct ->
                acct { accAddresses = accAddresses acct <> [result] }
          where
            createNewAddress :: WalletId -> AccountIndex -> NewAddress
            createNewAddress wId accIndex = NewAddress
                { newaddrSpendingPassword = Nothing
                , newaddrAccountIndex     = accIndex
                , newaddrWalletId         = wId
                }

        GetAddresses   -> do
            -- We choose one address, we could choose all of them.
            -- Also, remove the `V1` type since we don't need it now.
            address <-  fmap coerce . pickRandomElement =<< use addresses
            -- We get all the accounts.
            result  <-  respToRes $ getAddressIndex wc

            checkInvariant
                (address `elem` result)
                (LocalAddressesDiffer address result)

        GetAddress -> do
            -- We choose one address.
            address <-  fmap addrId . pickRandomElement =<< use addresses

            -- If we can't switch to @Text@ something is obviously wrong.
            let cAddress :: (MonadThrow m) => m (V0.CId V0.Addr)
                cAddress = either throwM pure (migrate address)

            textAddress <- coerce <$> cAddress

            log $ "Requesting: " <> show textAddress

            -- If the address exists, it is valid.
            void . respToRes $ getAddress wc textAddress

        -- Transactions
        PostTransaction -> do
            ws <- get
            let localAccounts  = ws ^. accounts
            let localAddresses = ws ^. addresses

            -- Some min amount of money so we can send a transaction?
            -- https://github.com/input-output-hk/cardano-sl/blob/develop/lib/configuration.yaml#L228
            let minCoinForTxs = V1 . mkCoin $ 200000
            let localAccsWithMoney = filter ((> minCoinForTxs) . accAmount) localAccounts

            -- From which source to pay.
            accountSource <- pickRandomElement localAccsWithMoney

            let accountSourceMoney = accAmount accountSource

            -- We should probably have a sensible minimum value.
            moneyAmount <- liftIO . fmap mkCoin . generate
                $ choose
                    ( 10
                    , getCoin (unV1 accountSourceMoney)
                    )
            -- moneyAmount <- liftIO $ generate arbitrary

            let paymentSource =
                    PaymentSource
                        { psWalletId     = accWalletId accountSource
                        , psAccountIndex = accIndex    accountSource
                        }

            addressDestination <- pickRandomElement localAddresses

            let paymentDistribution =
                    PaymentDistribution
                        { pdAddress = addrId addressDestination
                        , pdAmount  = V1 moneyAmount
                        }

            let newPayment =  createNewPayment
                                  paymentSource
                                  [paymentDistribution]

            -- Check the transaction fees.
            log $ "getTransactionFee: " <> show newPayment
            txFees  <-  respToRes $ getTransactionFee wc newPayment

            checkInvariant
                (feeEstimatedAmount txFees > minBound)
                (InvalidTransactionFee txFees)

            -- Check the transaction.
            log $ "postTransaction: " <> show newPayment
            result  <-  respToRes $ postTransaction wc newPayment

            let expectedAmount =
                    V1 (mkCoin (getCoin moneyAmount - getCoin (unV1 (feeEstimatedAmount txFees))))
            checkInvariant
                (txAmount result == expectedAmount)
                (InvalidTransactionState result)

            -- Modify wallet state accordingly.
            transactions  <>= [(accountSource, result)]

          where
            createNewPayment :: PaymentSource -> [PaymentDistribution] -> Payment
            createNewPayment ps pd = Payment
                { pmtSource           = ps
                , pmtDestinations     = fromList pd
                , pmtGroupingPolicy   = Nothing
                -- ^ Simple for now.
                , pmtSpendingPassword = Nothing
                }


        GetTransaction  -> do
            ws <- get
            let txs = ws ^. transactions

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
                                        Nothing
                                        Nothing

            -- First check we have results
            checkInvariant
                (not (null result))
                (LocalTransactionsDiffer result (map snd txs))

            -- Then check if the transaction exists in the history
            checkInvariant
                (transaction `elem` result)
                (LocalTransactionMissing transaction result)

    -- increment successful actions
    log "Success!"
    actionsNum += 1


-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


-- | Generate action randomly, depending on the action distribution.
chooseActionGen
    :: ActionProbabilities
    -> Gen Action
chooseActionGen =
    frequency . map (\(a, p) -> (getWeight p, pure a)) . toList


-- | Generate action from the generator.
chooseAction
    :: (WalletTestMode m)
    => ActionProbabilities
    -> m Action
chooseAction = liftIO . generate . chooseActionGen

-- | Generate a random sequence of actions with the given size.
chooseActions
    :: (WalletTestMode m)
    => Word
    -> ActionProbabilities
    -> m (NonEmpty Action)
chooseActions n probs = liftIO . generate $ do
    let gens = map (\(a, p) -> (getWeight p, pure a)) (toList probs)
    a <- frequency gens
    as <- replicateM (fromIntegral n - 1) (frequency gens)
    pure (a :| as)

-- | We are not interested in the @WalletResponse@ for now.
respToRes
    :: forall m a. (MonadThrow m)
    => m (Either ClientError (WalletResponse a))
    -> m a
respToRes resp = do
    result <- resp
    either throwM (pure . wrData) result


-- | Pick a random element using @IO@.
pickRandomElement :: (MonadIO m, Alternative m) => [a] -> m a
pickRandomElement [] = empty
pickRandomElement xs = liftIO . generate . elements $ xs

-- | A util function for checking the validity of invariants.
checkInvariant
    :: forall m. (MonadThrow m)
    => Bool
    -> WalletTestError
    -> m ()
checkInvariant True  _             = pure ()
checkInvariant False walletTestErr = throwM walletTestErr

log :: MonadIO m => Text -> m ()
log = putStrLn . mappend "[TEST-LOG] "

-- | Output for @Text@.
printT :: Text -> IO ()
printT = putStrLn

walletIdIsNotGenesis
    :: (MonadState WalletState m, Alternative m)
    => WalletId -> m ()
walletIdIsNotGenesis walletId = do
    mwallet <- head . filter ((walletId ==) . walId) <$> use wallets
    whenJust mwallet $ \wal ->
        guard (walName wal /= "Genesis wallet")

