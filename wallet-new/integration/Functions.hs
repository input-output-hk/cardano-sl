module Functions where

import Universum

import Control.Lens (ix)
import Control.Monad.Random
import Test.QuickCheck

import Cardano.Wallet.API.V1.Types (Wallet (..), New)

import Types
import Error

-- | The top function that we need to run in order
-- to test the backend.
runActionCheck
    :: (WIntTestMode m)
    => WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletState actionProb = do
    action <- chooseAction actionProb
    result <- runAction action

    validateInvariants result walletState

-- | Get action randomly, depending on the action distribution
chooseAction
    :: (WIntTestMode m)
    => ActionProbabilities
    -> m Action
chooseAction aProb = do
    prob <- liftIO chooseProb

    -- @choice@ from random-extras-0.2?
    let distributedActions :: ActionProbabilities
        distributedActions = concatMap replDistribution aProb

    maybe actionNotFound pure (distributedActions ^? ix prob . _1)
  where
    chooseProb :: IO Int
    chooseProb = getStdRandom (randomR (1,100))

    replDistribution
        :: (Action,Probability)
        -> ActionProbabilities
    replDistribution (action,prob) =
        replicate (getProbability prob) (action,prob)

    actionNotFound :: (MonadThrow m) => m Action
    actionNotFound = throwM $ Internal "The selected action was not found!"

-- | Here we run the actions. What we need from the other
-- side is the interpretation of this action. This
-- can be a typeclass with different interpretations.
runAction :: (WIntTestMode m) => Action -> m TestResult
runAction CreateWallet = createTestWallet
runAction _            = error "Implement"

createTestWallet :: forall m. (WIntTestMode m) => m TestResult
createTestWallet = do
    -- I guess we can try to use the new quickcheck types
    -- sometime in the future.
    newWallet <- liftIO $ generate arbitrary
    -- external call to either REST api or internal function
    externalCall newWallet
  where
    -- | TODO(ks): This needs to be an actual call.
    externalCall :: New Wallet -> m TestResult
    externalCall _ = liftIO $ NewWalletResult <$> generate arbitrary


-- | @WalletState@ is changed _only_ if the invariant is
-- valid.
validateInvariants
    :: (MonadThrow m)
    => TestResult
    -> WalletState
    -> m WalletState
validateInvariants (NewWalletResult w) ws = do
    checkInvariant (walBalance w == minBound) "Balance is not zero."
    -- Lenses will be required probably.
    pure $ WalletState
        { wallets    = wallets ws <> [w]
        , actionsNum = actionsNum ws + 1
        }
validateInvariants (ErrorResult e) _ = throwM $ Internal e

-- | A util function for checking the validity of invariants.
checkInvariant :: (MonadThrow m) => Bool -> Text -> m ()
checkInvariant True  _    = pure ()
checkInvariant False desc = throwM $ Internal desc
