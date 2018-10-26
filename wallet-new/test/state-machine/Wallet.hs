{-# LANGUAGE DataKinds           #-}
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
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wallet
  ( prop_wallet
  , prop_walletParallel
  , prop_fail
  , withWalletLayer
  )
  where

import           Universum

import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.TreeDiff (ToExpr (toExpr))
import           GHC.Generics (Generic, Generic1)
import           Test.QuickCheck (Gen, Property, arbitrary, frequency, generate,
                     ioProperty, oneof, (===))
import           Test.QuickCheck.Monadic (monadicIO)

import           Test.StateMachine
import           Test.StateMachine.Types (Command (..), Commands (..),
                     StateMachine)

import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (..),
                     UnitOfMeasure (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as DB
import           Cardano.Wallet.Kernel.Internal (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (mockNodeStateDef)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import qualified Cardano.Wallet.WalletLayer as WL
import qualified Cardano.Wallet.WalletLayer.Kernel as WL

import qualified Pos.Core as Core
import           Pos.Infra.InjectFail (mkFInjects)
import           Pos.Util.Wlog (Severity)
import qualified Pos.Wallet.Web.State.Storage as OldStorage


------------------------------------------------------------------------

-- Wallet actions

data Action (r :: * -> *)
    = ResetWalletA
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

data Model (r :: * -> *) = Model
    deriving (Eq, Show, Generic)

deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model

-- If you need more fine grained distribution, use preconditions
preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions _ ResetWalletA      = Top

transitions :: Model r -> Action r -> Response r -> Model r
transitions _ cmd res = case cmd of
    ResetWalletA -> Model

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions _ ResetWalletA ResetWalletR                          = Bot

------------------------------------------------------------------------

-- Action generator


generator :: Model Symbolic -> Gen (Action Symbolic)
-- if wallet has not been reset, then we should first reset it!
generator _ = pure ResetWalletA

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker _ = []

-- ------------------------------------------------------------------------
--
semantics :: WL.PassiveWalletLayer IO -> PassiveWallet -> Action Concrete -> IO (Response Concrete)
semantics pwl _ cmd = case cmd of
    ResetWalletA -> do
        WL.resetWalletState pwl
        return ResetWalletR

-- TODO: reuse withLayer function defined in wallet-new/test/unit/Test/Spec/Fixture.hs
withWalletLayer
          :: (WL.PassiveWalletLayer IO -> PassiveWallet -> IO a)
          -> IO a
withWalletLayer cc = do
    Keystore.bracketTestKeystore $ \keystore -> do
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
    devNull _ t = return ()

-- NOTE: I (akegalj) was not sure how library exactly uses mock so there is an explanation here https://github.com/advancedtelematic/quickcheck-state-machine/issues/236#issuecomment-431858389
-- NOTE: `mock` is not used in a current quickcheck-state-machine-0.4.2 so in practice we could leave it out. Its still in an experimental phase and there is a possibility it will be added in future versions of this library, so we won't leave it out just yet
mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _ ResetWalletA      = pure ResetWalletR

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

-- I was experimenting without forAllCommands to see how it would work
prop_fail :: (WL.PassiveWalletLayer IO, PassiveWallet) -> Property
prop_fail (pwl, pw) = ioProperty $ do
    let cmds = Commands
            [ Command ResetWalletA mempty
            ]
    pure $ monadicIO $ do
        print $ commandNamesInOrder cmds
        (hist, _, res) <- runCommands sm cmds
        prettyCommands sm hist $
            checkCommandNames cmds (res === Ok)
  where
    sm = stateMachine pwl pw

-- forAllCommands is using shrinking. When test fail with `postcondition _ GetWalletsA _ = Bot`
-- shrinking is going to start doing its job and I will get the report:
--
--        uncaught exception: SQLError
--        SQLite3 returned ErrorMisuse while attempting to perform prepare "BEGIN TRANSACTION": bad parameter or other API misuse
--        (after 7 tests and 1 shrink)
--          Commands { unCommands = [ Command ResetWalletA (fromList []) ] }
--
-- where I would expect the report similar to the one from prop_fail which looks like:
--
--        Falsifiable (after 1 test):
--          PostconditionFailed "BotC" /= Ok
--
-- is this problem in forAllCommands ? Or is it a problem in wallet backend?
-- or is it a problem with hunit, where we are doing all actions within the bracket
-- `around (withWalletLayer . curry)` (see Spec.hs)? Maybe hunit closed the db handle
-- before forAllCommands finished (if it forked into different thread)?
--
-- note that I have isolated wallet within binary wallet-reset-error (in wallet-new/cardano-sl-wallet-new.cabal)
-- and this binary works well (showcasing that reset wallet is actually working correctly).
-- So my primary suspect is `forAllCommands` (from quickcheck-state-machine) and/or `around` (from hspec)
prop_wallet :: (WL.PassiveWalletLayer IO, PassiveWallet) -> Property
prop_wallet (pwl, pw) = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
    print $ commandNamesInOrder cmds
    (hist, _, res) <- runCommands sm cmds
    prettyCommands sm hist $
        checkCommandNames cmds (res === Ok)
  where
    sm = stateMachine pwl pw

prop_walletParallel :: (WL.PassiveWalletLayer IO, PassiveWallet) -> Property
prop_walletParallel (pwl, pw) =
  forAllParallelCommands sm $ \cmds -> monadicIO $
    -- TODO: fix quickcheck with state machine pretty printer output (works well with tasty)
    prettyParallelCommands cmds =<< runParallelCommandsNTimes 100 sm cmds
  where
    sm = stateMachine pwl pw
