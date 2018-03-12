{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Functions where

import           Universum

import           Control.Lens ((+~))
import           Test.QuickCheck (Gen, arbitrary, elements, frequency, generate)

import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (Account (..), NewAccount (..), Wallet (..))

import           Cardano.Wallet.Client (ClientError (..), WalletClient (..))

import           Error
import           Types


-- | The top function that we need to run in order
-- to test the backend.
runActionCheck
    :: (WalletTestMode m)
    => WalletClient m
    -> WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletClient walletState actionProb = do
    action <- chooseAction actionProb
    runAction walletClient walletState action


-- | Here we run the actions.
runAction
    :: (WalletTestMode m)
    => WalletClient m
    -> WalletState
    -> Action
    -> m WalletState
-- Wallets
runAction wc ws  CreateWallet = do
    newWall <-  liftIO $ generate arbitrary
    result  <-  respToRes $ postWallet wc newWall

    ws'     <-  checkInvariant
                    ws
                    (walBalance result == minBound)
                    (Internal "Wallet balance is not zero.")

    -- Modify wallet state accordingly.
    pure $ ws'
        & wallets    .~ ws' ^. wallets <> [result]
        & actionsNum +~ 1

runAction wc ws GetWallets   = do
    -- We choose from the existing wallets.
    result  <-  respToRes $ getWallets wc

    ws'     <-  checkInvariant
                    ws
                    (length result == length (ws ^. wallets))
                    (Internal "Local wallets differs from server wallets.")

    -- No modification required.
    pure $ ws'
        & actionsNum +~ 1

runAction wc ws GetWallet    = do
    -- We choose from the existing wallets.
    wallet  <-  pickRandomElement (ws ^. wallets)
    result  <-  respToRes $ getWallet wc (walId wallet)

    ws'     <-  checkInvariant
                    ws
                    (walBalance result == minBound)
                    (Internal "Local wallet differs from server wallet.")

    -- No modification required.
    pure $ ws'
        & actionsNum +~ 1


-- Accounts
runAction wc ws  CreateAccount = do

    -- TODO(ks): Don't we need to know the wallet we want to add the account to?
    -- wallet     <- pickRandomElement localWallets
    let localWallets = ws ^. wallets

    -- Precondition, we need to have wallet in order
    -- to create an account.
    guard (length localWallets > 0)

    newAcc  <-  liftIO $ generate generateNewAccount
    result  <-  respToRes $ postAccount wc newAcc

    ws'     <-  checkInvariant
                    ws
                    (accAmount result == minBound)
                    (Internal "Account balance is not zero.")

    -- Modify wallet state accordingly.
    pure $ ws'
        & accounts   .~ ws' ^. accounts <> [result]
        & actionsNum +~ 1
  where
    -- | We don't want to memorize the passwords for now.
    generateNewAccount =
        NewAccount
            <$> pure Nothing
            <*> arbitrary

runAction wc ws GetAccounts   = do
    -- We choose from the existing wallets AND existing accounts.
    wallet  <-  pickRandomElement (ws ^. wallets)
    let walletId = walId wallet
    -- We get all the accounts.
    result  <-  respToRes $ getAccounts wc walletId

    ws'     <-  checkInvariant
                    ws
                    (length result == length (ws ^. accounts))
                    (Internal "Local accounts differ from server accounts.")

    -- Modify wallet state accordingly.
    pure $ ws'
        & actionsNum +~ 1

runAction wc ws GetAccount    = do
    -- We choose from the existing wallets AND existing accounts.
    account <-  pickRandomElement (ws ^. accounts)
    let walletId = accWalletId account

    result  <-  respToRes $ getAccount wc walletId (accIndex account)

    ws'     <-  checkInvariant
                    ws
                    (accAmount result == minBound)
                    (Internal "Local account differ from server account.")

    -- Modify wallet state accordingly.
    pure $ ws'
        & actionsNum +~ 1

-- Addresses
-- ...

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------


-- | Generate action randomly, depending on the action distribution.
chooseActionGen
    :: ActionProbabilities
    -> Gen Action
chooseActionGen aProb =
    frequency $ map (\(a, p) -> (getProbability p, pure a)) aProb


-- | Generate action from the generator.
chooseAction
    :: (WalletTestMode m)
    => ActionProbabilities
    -> m Action
chooseAction = liftIO . generate . chooseActionGen


-- | We are not interested in the @WalletResponse@ for now.
respToRes
    :: forall m a. (MonadThrow m)
    => m (Either ClientError (WalletResponse a))
    -> m a
respToRes resp = do
    result <- resp
    either throwM (pure . wrData) result


-- | Pick a random element using @IO@.
pickRandomElement :: (MonadIO m) => [a] -> m a
pickRandomElement = liftIO . generate . elements


-- | A util function for checking the validity of invariants.
checkInvariant
    :: forall m. (MonadThrow m)
    => WalletState
    -> Bool
    -> WalletTestError
    -> m WalletState
checkInvariant ws True  _             = pure ws
checkInvariant _  False walletTestErr = throwM walletTestErr


-- | Output for @Text@.
printT :: Text -> IO ()
printT = putStrLn

