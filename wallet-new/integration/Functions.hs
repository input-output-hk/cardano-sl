module Functions where

import Universum

import Control.Lens (ix, (+~))
import Control.Monad.Random
import Test.QuickCheck

import Cardano.Wallet.API.V1.Types (Wallet (..), New)

import Types
import Error


-- | The top function that we need to run in order
-- to test the backend.
runActionCheck
    :: (WalletTestMode m)
    => WalletState
    -> ActionProbabilities
    -> m WalletState
runActionCheck walletState actionProb = do
    action <- chooseAction actionProb
    result <- runAction action

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


-- | Get action randomly, depending on the action distribution
chooseActionRandom
    :: (WalletTestMode m)
    => ActionProbabilities
    -> m Action
chooseActionRandom aProb = do
    prob <- liftIO chooseProb

    -- @choice@ from random-extras-0.2?
    let distributedActions :: [Action]
        distributedActions = concatMap replDistribution aProb

    maybe actionNotFound pure (distributedActions ^? ix prob)
  where
    chooseProb :: IO Int
    chooseProb = getStdRandom (randomR (1,100))

    replDistribution
        :: (Action,Probability)
        -> [Action]
    replDistribution (action,prob) =
        replicate (getProbability prob) action

    actionNotFound :: (MonadThrow m) => m Action
    actionNotFound = throwM $ Internal "The selected action was not found!"


-- | Here we run the actions. What we need from the other
-- side is the interpretation of this action. This
-- can be a typeclass with different interpretations.
runAction :: (WalletTestMode m) => Action -> m TestResult
runAction CreateWallet = createTestWallet
runAction _            = error "Implement"


createTestWallet :: forall m. (WalletTestMode m) => m TestResult
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
validateInvariants (ErrorResult e) _ = throwM $ Internal e
validateInvariants (NewWalletResult w) ws = do
    checkInvariant (walBalance w == minBound) "Balance is not zero."

    -- Modify wallet state accordingly.
    pure $ ws
        & wallets    .~ ws ^. wallets <> [w]
        & actionsNum +~ 1


-- | A util function for checking the validity of invariants.
checkInvariant :: (MonadThrow m) => Bool -> Text -> m ()
checkInvariant True  _    = pure ()
checkInvariant False desc = throwM $ Internal desc
