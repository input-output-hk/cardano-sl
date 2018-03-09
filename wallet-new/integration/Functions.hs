module Functions where

import Universum

import Control.Lens ((+~))
import Test.QuickCheck (Gen, generate, arbitrary, frequency, elements)

import Cardano.Wallet.API.Response (WalletResponse (..))
import Cardano.Wallet.API.V1.Types (Wallet (..))

import Cardano.Wallet.Client (WalletClient (..), ClientError (..))

import Types
import Error


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
    result <- runAction walletClient walletState action

    validateInvariants result walletState


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


-- | Here we run the actions. What we need from the other
-- side is the interpretation of this action. This
-- can be a typeclass with different interpretations.
runAction
    :: (WalletTestMode m)
    => WalletClient m
    -> WalletState
    -> Action
    -> m TestResult
-- Wallets
runAction wc _  CreateWallet = do
    newWallet <- liftIO $ generate arbitrary
    result    <- postWallet wc newWallet

    pure $ case result of
        Left  e   -> ErrorResult e
        Right res -> CreateWalletResult $ wrData res

runAction wc ws GetWallet    = do
    -- We choose from the existing wallets.
    wallet   <- liftIO $ generate $ elements (ws ^. wallets)
    result   <- getWallet wc (walId wallet)

    pure $ case result of
        Left  e   -> ErrorResult e
        Right res -> GetWalletResult $ wrData res

-- Accounts
runAction wc _  CreateAccount = do
    newAccount <- liftIO $ generate arbitrary
    result     <- postAccount wc newAccount

    pure $ case result of
        Left  e   -> ErrorResult e
        Right res -> CreateAccountResult $ wrData res

runAction wc ws GetAccounts   = do
    -- We choose from the existing wallets AND existing accounts.
    wallet   <- liftIO $ generate $ elements (ws ^. wallets)
    let walletId = walId wallet
    -- We get all the accounts.
    accounts <- getAccounts wc walletId

    pure $ case accounts of
        Left  e   -> ErrorResult e
        Right res -> GetAccountsResult $ wrData res

runAction _ _ _             = error "Implement"


-- | @WalletState@ is changed _only_ if the invariant is
-- valid.
validateInvariants
    :: (MonadThrow m)
    => TestResult
    -> WalletState
    -> m WalletState
-- Maybe output the wallet state if something goes wrong?
validateInvariants (ErrorResult (ClientWalletError e)) _  = throwM e
validateInvariants (ErrorResult (ClientHttpError e))   _  = throwM e
validateInvariants (ErrorResult (UnknownError e))      _  = throwM e

validateInvariants (CreateWalletResult w) ws = do
    checkInvariant (walBalance w == minBound) "Balance is not zero."

    -- Modify wallet state accordingly.
    pure $ ws
        & wallets    .~ ws ^. wallets <> [w]
        & actionsNum +~ 1

validateInvariants (GetWalletResult w) ws = do

    checkInvariant (walBalance w == minBound) "Balance is not zero."
    -- No modification required.
    pure $ ws
        & actionsNum +~ 1


-- | A util function for checking the validity of invariants.
checkInvariant :: (MonadThrow m) => Bool -> Text -> m ()
checkInvariant True  _    = pure ()
checkInvariant False desc = throwM $ Internal desc
