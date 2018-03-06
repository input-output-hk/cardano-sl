{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
    ( Probability
    , createProbability
    , getProbability
    , Action (..)
    , WalletState (..)
    , wallets
    , actionsNum
    , TestResult (..)
    , ActionWalletState
    , ActionProbabilities
    , WalletTestMode
    -- * EDSL
    , Expr (..)
    , createActionDistribution
    , createActionHandler
    , createActionError
    , testEval
    ) where

import Universum

import Control.Lens (makeLenses)

import Cardano.Wallet.API.V1.Types (Wallet)

import Error


type WalletTestMode m =
    ( MonadIO m
    , MonadThrow m
    )

-- | The probability type that captures the chance of
-- our random action selection choosing a specific @Action@.
-- The number is restricted to 1 - 100 - see @createProbability@.
data Probability = Probability { getProbability :: Int }
    deriving (Show, Eq)


-- | Safe constructor that checks the values.
createProbability :: Int -> Probability
createProbability prob
    | prob <= 0 || prob > 100 = error "Invalid range. 0 - 100."
    | otherwise               = Probability prob


-- | Actions that can be called from the test.
data Action
    = CreateWallet
    | GetWallet
    | CreateAccount
    | CreateTransaction
    | GetTransaction
    deriving (Show, Eq)


-- | The type that defines the probabilites of the actions
-- TODO(ks): We could create a custom constructor with valid
-- values only.
-- Add invariant?
type ActionProbabilities = [(Action, Probability)]


-- | State of the wallet while testing, from the client side.
-- We require this so we can check for the invariants and
-- keep track of some interesting information.
data WalletState = WalletState
    { _wallets       :: [Wallet]
    , _actionsNum    :: Int
    } deriving (Show, Eq, Generic)


makeLenses ''WalletState


-- | The type that has the action probabilities to execute along
-- with the current @WalletState@ from the client perspective.
-- Yes, I know, @MonadState@, but we can always switch to that.
type ActionWalletState = (WalletState, ActionProbabilities)


-- | The type we use to capture the result of the @Action@.
-- TODO(ks): I have some ideas how to extend this, so I
-- just created a type, it's useless otherwise.
-- For example, we can record the time it took for the endpoint
-- to respond, which will help us measure regressions.
-- The smart thing about this is that it's going to capture
-- regressions non-linearly - by exploring the state space
-- randomly, we may find out some conditions which slow our code
-- down significantly. Since we don't have a lot of time,
-- I just keep the options open (there are other ideas as well).
data TestResult
    = NewWalletResult Wallet
    | ErrorResult Text
    deriving (Show, Eq)


-- | This is helpful.
type ActionResult = Action -> TestResult
type Invariant    = [ActionResult -> Bool]


-- | The EDSL for using this.
data Expr
    = ActionDistribution [(Action,Probability)]
    | ActionHandler Action ActionResult Invariant
    | ActionError WalletTestError
    deriving (Generic)


createActionDistribution :: [(Action,Probability)] -> Expr
createActionDistribution = ActionDistribution


createActionHandler :: Action -> ActionResult -> Invariant -> Expr
createActionHandler = ActionHandler


createActionError :: WalletTestError -> Expr
createActionError = ActionError


-- | Testing evaluation.
testEval :: Expr -> Text
testEval (ActionDistribution aDistr) = "ActionDistribution " <> show aDistr
testEval (ActionHandler action _ _)  = "ActionHandler " <> show action
testEval (ActionError actionErr)     = "ActionError " <> show actionErr


