{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Functions where

import           Prelude (error)
import           Universum hiding (error)

import           Data.Coerce (coerce)
import           Data.List (delete)
import           Data.List.NonEmpty (fromList)

import           Control.Lens (at, each, filtered, uses, (+=), (.=), (<>=), (?=))
import           Test.QuickCheck (Gen, arbitrary, elements, frequency, generate)

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex, AccountUpdate (..),
                                              AssuranceLevel (..), EstimatedFees (..),
                                              NewAccount (..), NewAddress (..), NewWallet (..),
                                              PasswordUpdate (..), Payment (..),
                                              PaymentDistribution (..), PaymentSource (..),
                                              Transaction (..), V1 (..),
                                              Wallet (..), WalletAddress (..), WalletId,
                                              WalletOperation (..), WalletUpdate (..))

import           Cardano.Wallet.API.V1.Migration.Types (migrate)
import           Cardano.Wallet.Client (ClientError (..), WalletClient (..), getAccounts,
                                        getAddressIndex, getTransactionIndex, getWallets,
                                        hoistClient)

import           Pos.Core (mkCoin)
import           Pos.Util (maybeThrow)
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

-- | Attempt each action in the list. If an action fails, ignore the
-- failure.
tryAll :: Alternative f => [f a] -> f a
tryAll []     = empty
tryAll (x:xs) = foldr (\act acc -> act *> acc <|> acc) x xs

-- | Here we run the actions.
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
runAction
    :: (WalletTestMode m, HasCallStack, MonadState WalletState m)
    => WalletClient m
    -> Action
    -> m ()
-- Wallets
runAction wc PostWallet = do
    newPassword <- liftIO $ generate arbitrary
    newWall     <- liftIO $ generate $ generateNewWallet newPassword
    result      <- respToRes $ postWallet wc newWall

    checkInvariant
        (walBalance result == minBound)
        (WalletBalanceNotZero result)

    -- Modify wallet state accordingly.
    wallets    <>= [result]
    walletsPass . at result ?= newPassword
    actionsNum += 1
  where
    generateNewWallet spendPass =
        NewWallet
            <$> arbitrary
            <*> pure (Just spendPass)
            <*> arbitrary
            <*> pure "Wallet"
            <*> pure CreateWallet

runAction wc GetWallets   = do
    ws      <- get
    -- We choose from the existing wallets.
    result  <-  respToRes $ getWallets wc

    checkInvariant
        (length result == length (ws ^. wallets))
        (LocalWalletsDiffers result)

    -- No modification required.
    actionsNum += 1

runAction wc GetWallet    = do
    ws <- get

    let localWallets = ws ^. wallets

    -- The precondition is that we need to have wallets.
    guard (not (null localWallets))

    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement localWallets
    result  <-  respToRes $ getWallet wc (walId wallet)

    checkInvariant
        (walBalance result == minBound)
        (LocalWalletDiffers result)

    -- No modification required.
    actionsNum += 1

runAction wc DeleteWallet = do
    localWallets <- use wallets

    -- The precondition is that we need to have wallets.
    guard (not (null localWallets))

    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement localWallets

    -- If we don't have any http client errors, the delete was a success.
    _       <-  either throwM pure =<< deleteWallet wc (walId wallet)

    -- Just in case, let's check if it's still there.
    result  <-  respToRes $ getWallets wc

    checkInvariant
        (all ((/=) wallet) localWallets)
        (LocalWalletsDiffers result)

    -- Modify wallet state accordingly.
    wallets    .= delete wallet localWallets
    actionsNum += 1

runAction wc UpdateWallet = do

    localWallets <- use wallets

    -- The precondition is that we need to have wallets.
    guard (not (null localWallets))

    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement localWallets

    let walletId = walId wallet

    let newWallet =
            WalletUpdate
                { uwalAssuranceLevel = NormalAssurance
                , uwalName           = "Wallet name " <> (show walletId)
                }

    result  <-  respToRes $ updateWallet wc walletId newWallet

    -- Modify wallet state accordingly.
    wallets . each . filtered (== wallet) .= result
    actionsNum += 1

runAction wc UpdateWalletPass = do

    localWallets <- use wallets

    -- The precondition is that we need to have wallets.
    guard (not (null localWallets))

    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement localWallets

    let walletId = walId wallet

    -- Get the old wallet password.
    oldWalletPass <- use (walletsPass . at wallet)

    walletPass  <- maybeThrow (WalletPassMissing wallet) oldWalletPass
    newPassword <- liftIO $ generate arbitrary

    -- Create a password update.
    let newPasswordUpdate =
            PasswordUpdate
                { pwdOld = walletPass
                , pwdNew = newPassword
                }

    result  <-  respToRes $ updateWalletPassword wc walletId newPasswordUpdate

    -- Modify wallet state accordingly.
    wallets . each . filtered (== wallet) .= result
    walletsPass . at wallet ?= newPassword
    actionsNum += 1

-- Accounts
runAction wc PostAccount = do

    localWallets <- use wallets

    -- Precondition, we need to have wallet in order
    -- to create an account.
    guard (not (null localWallets))

    wallet     <- pickRandomElement localWallets
    newAcc  <-  liftIO $ generate generateNewAccount
    result  <-  respToRes $ postAccount wc (walId wallet) newAcc

    checkInvariant
        (accAmount result == minBound)
        (AccountBalanceNotZero result)

    -- Modify wallet state accordingly.
    accounts   <>= [result]
    actionsNum += 1
  where
    -- | We don't want to memorize the passwords for now.
    generateNewAccount =
        NewAccount
            <$> pure Nothing
            <*> arbitrary

runAction wc GetAccounts   = do
    -- We choose from the existing wallets AND existing accounts.
    wallet  <-  pickRandomElement =<< use wallets
    let walletId = walId wallet
    -- We get all the accounts.
    result  <-  respToRes $ getAccounts wc walletId

    acctLength <- uses accounts length
    checkInvariant
        (length result == acctLength)
        (LocalAccountsDiffers result)

    actionsNum += 1

runAction wc GetAccount    = do
    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement =<< use accounts
    let walletId = accWalletId account

    result  <-  respToRes $ getAccount wc walletId (accIndex account)

    checkInvariant
        (accAmount result == minBound)
        (LocalAccountDiffers result)

    -- Modify wallet state accordingly.
    actionsNum += 1

runAction wc DeleteAccount = do
    localAccounts <- use accounts

    -- The precondition is that we need to have accounts.
    guard (not (null localAccounts))

    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement localAccounts
    let walletId = accWalletId account

    -- If we don't have any http client errors, the delete was a success.
    _ <- either throwM pure =<< deleteAccount wc walletId (accIndex account)

    -- Just in case, let's check if it's still there.
    result  <-  respToRes $ getAccounts wc walletId

    checkInvariant
        (all ((/=) account) localAccounts)
        (LocalAccountsDiffers result)

    -- Modify wallet state accordingly.
    accounts   .= delete account localAccounts
    actionsNum += 1

runAction wc UpdateAccount = do
    localAccounts <- use accounts

    -- The precondition is that we need to have accounts.
    guard (not (null localAccounts))

    -- We choose from the existing wallets.
    account <-  pickRandomElement localAccounts

    let walletId  = accWalletId account
    let accountId = accIndex account

    let newAccount =
            AccountUpdate
                { uaccName = "Account name " <> (show accountId)
                }

    result  <-  respToRes $ updateAccount wc walletId accountId newAccount

    -- Modify wallet state accordingly.
    accounts . each . filtered (== account) .= result
    actionsNum += 1

-- Addresses
runAction wc PostAddress = do
    -- The precondition is that we must have accounts.
    -- If we have accounts, that presupposes that we have wallets,
    -- which is the other thing we need here.
    localAccounts <- use accounts

    guard (not (null localAccounts))

    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement localAccounts
    let walletId = accWalletId account

    let newAddress = createNewAddress walletId (accIndex account)

    result  <-  respToRes $ postAddress wc newAddress

    checkInvariant
        (addrBalance result == minBound)
        (AddressBalanceNotZero result)

    -- Modify wallet state accordingly.
    addresses  <>= [result]
    actionsNum += 1
  where
    createNewAddress :: WalletId -> AccountIndex -> NewAddress
    createNewAddress wId accIndex = NewAddress
        { newaddrSpendingPassword = Nothing
        , newaddrAccountIndex     = accIndex
        , newaddrWalletId         = wId
        }

runAction wc GetAddresses   = do
    -- We choose one address, we could choose all of them.
    -- Also, remove the `V1` type since we don't need it now.
    address <-  fmap coerce . pickRandomElement =<< use addresses
    -- We get all the accounts.
    result  <-  respToRes $ getAddressIndex wc

    checkInvariant
        (address `elem` result)
        (LocalAddressesDiffer address result)

    -- Modify wallet state accordingly.
    actionsNum += 1

runAction wc GetAddress     = do
    -- We choose one address.
    address <-  fmap addrId . pickRandomElement =<< use addresses

    -- If we can't switch to @Text@ something is obviously wrong.
    let cAddress :: (MonadThrow m) => m (V0.CId V0.Addr)
        cAddress = either throwM pure (migrate address)

    textAddress <- coerce <$> cAddress

    -- If the address exists, it is valid.
    _  <-  respToRes $ getAddress wc textAddress

    -- Modify wallet state accordingly.
    actionsNum += 1

-- Transactions
runAction wc PostTransaction = do
    ws <- get
    let localAccounts  = ws ^. accounts
    let localAddresses = ws ^. addresses

    -- Some min amount of money so we can send a transaction?
    -- https://github.com/input-output-hk/cardano-sl/blob/develop/lib/configuration.yaml#L228
    let minCoinForTxs = V1 . mkCoin $ 200000
    let localAccsWithMoney = filter ((> minCoinForTxs) . accAmount) localAccounts

    -- | The preconditions we need to generate a transaction.
    -- We need to have an account and an address.
    -- We also need money to execute a transaction.
    guard (not (null localAccounts))
    guard (not (null localAddresses))
    guard (not (null localAccsWithMoney))

    -- From which source to pay.
    accountSource <- pickRandomElement localAccsWithMoney

    let _accountSourceMoney = accAmount accountSource

    -- We should probably have a sensible minimum value.
    -- moneyAmount <- liftIO $ mkCoin $ generate $ choose (0, getCoin accountSourceMoney)
    moneyAmount <- liftIO $ generate arbitrary

    let paymentSource =
            PaymentSource
                { psWalletId     = accWalletId accountSource
                , psAccountIndex = accIndex    accountSource
                }

    addressDestination <- pickRandomElement localAddresses

    let paymentDistribution =
            PaymentDistribution
                { pdAddress = addrId addressDestination
                , pdAmount  = moneyAmount
                }

    let newPayment =  createNewPayment
                          paymentSource
                          [paymentDistribution]

    -- Check the transaction fees.
    txFees  <-  respToRes $ getTransactionFee wc newPayment

    checkInvariant
        (feeEstimatedAmount txFees > minBound)
        (InvalidTransactionFee txFees)

    -- Check the transaction.
    result  <-  respToRes $ postTransaction wc newPayment

    checkInvariant
        (txAmount result == moneyAmount)
        (InvalidTransactionState result)

    -- Modify wallet state accordingly.
    transactions  <>= [(accountSource, result)]
    actionsNum    += 1

  where
    createNewPayment :: PaymentSource -> [PaymentDistribution] -> Payment
    createNewPayment ps pd = Payment
        { pmtSource           = ps
        , pmtDestinations     = fromList pd
        , pmtGroupingPolicy   = Nothing
        -- ^ Simple for now.
        , pmtSpendingPassword = Nothing
        }


runAction wc GetTransaction  = do
    ws <- get
    let txs = ws ^. transactions

    -- We need to have transactions in order to test this endpoint.
    guard (not (null txs))

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
        (LocalTransactionsDiffer result)

    -- Then check if the transaction exists in the history
    checkInvariant
        (transaction `elem` result)
        (LocalTransactionMissing transaction result)

    -- Modify wallet state accordingly.
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
pickRandomElement :: (MonadIO m, HasCallStack) => [a] -> m a
pickRandomElement [] = error "called with empty list"
pickRandomElement xs = liftIO . generate . elements $ xs

-- | A util function for checking the validity of invariants.
checkInvariant
    :: forall m. (MonadThrow m)
    => Bool
    -> WalletTestError
    -> m ()
checkInvariant True  _             = pure ()
checkInvariant False walletTestErr = throwM walletTestErr


-- | Output for @Text@.
printT :: Text -> IO ()
printT = putStrLn
