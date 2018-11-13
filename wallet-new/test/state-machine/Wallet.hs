{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wallet
  ( prop_wallet
  , withWalletLayer
  )
  where

import           Universum

import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.TreeDiff (ToExpr (toExpr))
import           GHC.Generics (Generic, Generic1)
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, Property,
                     elements, frequency, (===))
import           Test.QuickCheck.Monadic (monadicIO)

import           Test.StateMachine
import           Test.StateMachine.Types (Command (..), Commands (..),
                     StateMachine)

import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Cardano.Wallet.API.Request (RequestParams (..))
import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
import           Cardano.Wallet.API.Request.Pagination (PaginationParams (..))
import           Cardano.Wallet.API.Response (WalletResponse (wrData),
                     fromSlice)
import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Client (paginateAll)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as DB
import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import qualified Cardano.Wallet.WalletLayer as WL
import qualified Cardano.Wallet.WalletLayer.Kernel as WL

import qualified Pos.Binary.Class as BI
import qualified Pos.Core as Core
import qualified Pos.Crypto.Signing as Core
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util.Wlog (Severity)
import qualified Pos.Wallet.Web.State.Storage as OldStorage


------------------------------------------------------------------------

-- Wallet actions

data Action (r :: * -> *)
    = ResetWalletA
    | CreateWalletA WL.CreateWallet
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    | CreateWalletR (Either WL.CreateWalletError V1.Wallet)
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

data Model (r :: * -> *) = Model
    -- TODO: don't use touple here, use record
    { mWallets     :: [(V1.Wallet, Maybe V1.SpendingPassword)]
    , mUnhappyPath :: Int
    , mReset       :: Bool
    }
    deriving (Eq, Show, Generic)

deriving instance ToExpr (V1.V1 Core.Timestamp)
deriving instance ToExpr Core.Coin
deriving instance ToExpr Core.Timestamp
deriving instance ToExpr (V1.V1 Core.Coin)
deriving instance ToExpr (V1.V1 V1.AddressOwnership)
deriving instance ToExpr V1.AddressOwnership
instance ToExpr (V1.V1 Core.Address) where
    -- TODO: check is this viable solution
    toExpr = toExpr @String . show . BI.encode . V1.unV1
deriving instance ToExpr V1.WalletAddress
deriving instance ToExpr V1.AccountIndex
deriving instance ToExpr V1.Account
deriving instance ToExpr V1.Wallet
deriving instance ToExpr V1.WalletId
deriving instance ToExpr V1.AssuranceLevel
deriving instance ToExpr V1.SyncState
deriving instance ToExpr V1.WalletType
deriving instance ToExpr V1.SyncProgress
deriving instance ToExpr V1.SyncPercentage
deriving instance ToExpr V1.SyncThroughput
deriving instance ToExpr OldStorage.SyncThroughput
deriving instance ToExpr V1.EstimatedCompletionTime
instance ToExpr (V1.V1 Core.PassPhrase) where
    -- TODO: check is this viable solution
    toExpr = toExpr @String . show . BI.encode . V1.unV1
deriving instance ToExpr Core.BlockCount
deriving instance ToExpr (MeasuredIn 'Milliseconds Word)
deriving instance ToExpr (MeasuredIn 'BlocksPerSecond Word)
deriving instance ToExpr (MeasuredIn 'Percentage100 Word8)
deriving instance ToExpr (MeasuredIn 'BlocksPerSecond OldStorage.SyncThroughput)
instance ToExpr Microsecond where
    toExpr = toExpr . toMicroseconds
deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model mempty 0 False

-- If you need more fine grained distribution, use preconditions
preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions _ ResetWalletA      = Top
-- NOTE: with this mechanism we are forcing ResetWalletA to be first action
-- in an action list generated. We need this because we are reusing same
-- db through all quickcheck runs - so we have to ensure ResetActionA is run
-- before any other action. Implementation by precondition is good enough and
-- generator won't need to try too many options until it reaches ResetActionA.
-- An alternative solution would be to modify `forAllCommands` in this way:
--
-- ```
-- forAllCommands' sm n action = forAllCommands sm n $ \cmds -> action $ Commands [Command ResetWalletA mempty] <> cmds
-- ```
--
-- The above would work a bit more performant (as we don't have failing preconditions)
-- but we are hacking around to lib internals which is not so idiomatic.
preconditions (Model _ _ True) action = case action of
    ResetWalletA    -> Top
    CreateWalletA _ -> Top

    -- NOTE: don't use catch all pattern like
    --
    -- (_, _) ->
    --
    -- as it will most likely bite us.

-- if wallet is not reset then we shouldn't continue
preconditions (Model _ _ False) _   = Bot

transitions :: Model r -> Action r -> Response r -> Model r
transitions model@Model{..} cmd res = case (cmd, res) of
    (ResetWalletA, ResetWalletR) -> Model mempty 0 True
    (ResetWalletA, _) -> shouldNotBeReachedError
    (CreateWalletA (WL.CreateWallet V1.NewWallet{..}), CreateWalletR (Right wallet)) ->
        model { mWallets = (wallet, newwalSpendingPassword) : mWallets }
    (CreateWalletA _, CreateWalletR (Left _)) -> increaseUnhappyPath
    (CreateWalletA _, _) -> shouldNotBeReachedError

    -- NOTE: don't use catch all pattern like
    --
    -- (_, _) ->
    --
    -- as it will most likely bite us.
  where
    -- TODO: use postcondition that ration of unhappy paths has to be expected (ie, similar to test coverage)
    -- If number of unhappy paths is too high something might go wrong and our tests are not covering enough happy paths (and vice versa)?
    increaseUnhappyPath = model { mUnhappyPath = mUnhappyPath + 1 }
    shouldNotBeReachedError = error "This branch should not be reached!"

-- TODO: use unit tests in wallet-new/test/unit for inspiration
postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions Model{..} cmd res = case (cmd, res) of
    (ResetWalletA, ResetWalletR)               -> Top
    (ResetWalletA, _)                          -> shouldNotBeReachedError
    -- It should be expected for a wallet creation to fail sometimes, but not currently in our tests.
    (CreateWalletA _, CreateWalletR (Left _))  -> Bot
    -- TODO: check that wallet request and wallet response contain same attributes
    -- that we have created intended wallet
    -- FIXME: this postcondition can be made much stronger!
    -- Created wallet shouldn't be present in the model
    (CreateWalletA _, CreateWalletR (Right V1.Wallet{..})) -> Predicate $ NotElem walId (map (V1.walId . fst) mWallets)
    (CreateWalletA _, _) -> shouldNotBeReachedError

    -- NOTE: don't use catch all pattern like
    --
    -- (_, _) ->
    --
    -- as it will most likely bite us.

  where
    shouldNotBeReachedError = error "This branch should not be reached!"

------------------------------------------------------------------------

-- Action generator

-- Same as quickchecks elements but if list is empty it will generate
-- arbitrary element
genElements :: Arbitrary a => [a] -> Gen a
genElements [] = arbitrary
genElements xs = elements xs

genNewWalletRq :: Gen V1.NewWallet
genNewWalletRq = do
    spendingPassword <- frequency [(20, pure Nothing), (80, Just <$> arbitrary)]
    assuranceLevel   <- arbitrary
    walletName       <- arbitrary
    mnemonic <- arbitrary @(BIP39.Mnemonic 12)
    return $ V1.NewWallet (V1.BackupPhrase mnemonic)
                          spendingPassword
                          assuranceLevel
                          walletName
                          V1.CreateWallet

generator :: Model Symbolic -> Gen (Action Symbolic)
-- if wallet has not been reset, then we should first reset it!
generator (Model _ _ False) = pure ResetWalletA
-- we would like to create a wallet after wallet reset
generator (Model [] _ True) = CreateWalletA . WL.CreateWallet <$> genNewWalletRq
generator model@Model{..} = frequency
    [ (1, pure ResetWalletA)
    , (1, CreateWalletA . WL.CreateWallet <$> genNewWalletRq)
    -- ... other actions should go here
    ]

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker _ = []

-- ------------------------------------------------------------------------
--
semantics :: WL.PassiveWalletLayer IO -> PassiveWallet -> Action Concrete -> IO (Response Concrete)
semantics pwl _ cmd = case cmd of
    ResetWalletA -> do
        -- TODO: check how it will behave if exception is raised here!
        WL.resetWalletState pwl
        return ResetWalletR
    CreateWalletA cw -> do
        w <- CreateWalletR <$> WL.createWallet pwl cw
        print "Concrete wallet created" -- show w
        pure w

withWalletLayer
          :: (WL.PassiveWalletLayer IO -> PassiveWallet -> IO a)
          -> IO a
withWalletLayer cc = do
    Keystore.bracketTestKeystore $ \keystore -> do
        -- TODO: can we use fault injection in wallet to test expected failure cases?
        mockFInjects <- mkFInjects mempty
        WL.bracketPassiveWallet
            Kernel.UseInMemory
            devNull
            keystore
            mockNodeStateDef
            mockFInjects
            cc
  where
    devNull :: Severity -> Text -> IO ()
    devNull _ _ = return ()

-- NOTE: I (akegalj) was not sure how library exactly uses mock so there is an explanation here https://github.com/advancedtelematic/quickcheck-state-machine/issues/236#issuecomment-431858389
-- NOTE: `mock` is not used in a current quickcheck-state-machine-0.4.2 so in practice we could leave it out. Its still in an experimental phase and there is a possibility it will be added in future versions of this library, so we won't leave it out just yet
mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _ ResetWalletA      = pure ResetWalletR
-- TODO: add mocking up creating an actual wallet
-- For example you can take one from the model, just change wallet id
mock _ (CreateWalletA _) = pure $ CreateWalletR (Left $ WL.CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)

------------------------------------------------------------------------

-- TODO: model invariant?
-- TODO: model distribution?
stateMachine :: WL.PassiveWalletLayer IO -> PassiveWallet -> StateMachine Model Action IO Response
stateMachine pwl pw =
    StateMachine
        initModel
        transitions
        preconditions
        postconditions
        Nothing
        generator
        Nothing
        shrinker
        (semantics pwl pw)
        mock

prop_wallet :: WL.PassiveWalletLayer IO -> PassiveWallet -> Property
prop_wallet pwl pw = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
    print $ commandNamesInOrder cmds
    (hist, _, res) <- runCommands sm cmds
    prettyCommands sm hist $
        checkCommandNames cmds (res === Ok)
  where
    sm = stateMachine pwl pw
