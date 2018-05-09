{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TypeFamilies    #-}

module Types
    ( Weight (..)
    , Action (..)
    , WalletState (..)
    , lastAction
    , wallets
    , nonGenesisWallets
    , walletsPass
    , accounts
    , addresses
    , transactions
    , actionsNum
    , successActions
    , ActionWalletState
    , ActionProbabilities
    , WalletTestMode
    ) where

import           Universum

import           Control.Lens (Getter, makeLenses, to)
import           Data.Aeson (ToJSON (..))

import           Cardano.Wallet.API.V1.Types (Account, SpendingPassword, Transaction, Wallet (..),
                                              WalletAddress, WalletId (..))

-- | Ideally, we would put @MonadGen@ here and remove @MonadIO@,
-- but it's better to see how the client fits in the end.
type WalletTestMode m =
    ( MonadIO m
    , MonadCatch m
    , MonadPlus m
    )

-- | The probability type that captures the chance of
-- our random action selection choosing a specific 'Action'.
--
-- An 'Action' is paired with a 'Weight'. All of the 'Weight's are summed
-- up, and the eventual probability is the weight divided by the total
-- weight.
newtype Weight = Weight { getWeight :: Int }
    deriving (Show, Eq)

-- | Actions that can be called from the test.
data Action
    = PostWallet
    | GetWallets
    | GetWallet
    | DeleteWallet
    | UpdateWallet
    | UpdateWalletPass

    | PostAccount
    | GetAccounts
    | GetAccount
    | DeleteAccount
    | UpdateAccount

    | PostAddress
    | GetAddresses
    | GetAddress

    | PostTransaction
    | GetTransaction

    | NoOp
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | The type that defines the probabilites of the actions
-- TODO(ks): We could create a custom constructor with valid
-- values only.
-- Add invariant?
type ActionProbabilities = NonEmpty (Action, Weight)

-- | State of the wallet while testing, from the client side.
-- We require this so we can check for the invariants and
-- keep track of some interesting information.
data WalletState = WalletState
    { _lastAction       :: Action
    , _wallets          :: [Wallet]
    , _walletsPass      :: Map WalletId SpendingPassword
    , _accounts         :: [Account]
    , _addresses        :: [WalletAddress]
    , _transactions     :: [(Account, Transaction)]
    -- ^ A tuple since for now we can't get @Wallet@ or
    -- @Account@ with a @Transaction@.
    , _actionsNum       :: Int
    -- ^ The count of actions that have been performed thus far.
    , _successActions   :: [Action]
    -- ^ Successful actions successful tests that have run so far.
    } deriving (Show, Eq, Generic)

makeLenses ''WalletState

nonGenesisWallets :: Getter WalletState [Wallet]
nonGenesisWallets = wallets . to (filter (("Genesis wallet" /=) . walName))

-- | The type that has the action probabilities to execute along
-- with the current @WalletState@ from the client perspective.
-- Yes, I know, @MonadState@, but we can always switch to that.
type ActionWalletState = (WalletState, ActionProbabilities)

-- JSON
instance ToJSON Action

instance ToJSON WalletState

