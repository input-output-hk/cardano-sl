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
    | CreateWalletA WL.CreateWallet
    | GetWalletsA
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    | CreateWalletR (Either WL.CreateWalletError V1.Wallet)
    | GetWalletsR (DB.IxSet V1.Wallet)
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

data Model (r :: * -> *) = Model
    { mWallets :: [V1.Wallet]
    , mReset   :: Bool
    }
    deriving (Eq, Show, Generic)

-- NOTE: this is use for diffing datatypes for pretty printer
deriving instance ToExpr (V1.V1 Core.Timestamp)
deriving instance ToExpr Core.Coin
deriving instance ToExpr Core.Timestamp
deriving instance ToExpr (V1.V1 Core.Coin)
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
deriving instance ToExpr Core.BlockCount
deriving instance ToExpr (MeasuredIn 'Milliseconds Word)
deriving instance ToExpr (MeasuredIn 'BlocksPerSecond Word)
deriving instance ToExpr (MeasuredIn 'Percentage100 Word8)
deriving instance ToExpr (MeasuredIn 'BlocksPerSecond OldStorage.SyncThroughput)
instance ToExpr Microsecond where
    toExpr = toExpr . toMicroseconds
deriving instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model [] False

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
preconditions (Model _ True) action = case action of
    ResetWalletA    -> Top
    CreateWalletA _ -> Top
    GetWalletsA     -> Top
-- if wallet is not reset then we shouldn't continue
preconditions (Model _ False) _   = Bot

transitions :: Model r -> Action r -> Response r -> Model r
transitions model@Model{..} cmd res = case cmd of
    ResetWalletA -> Model [] True
    CreateWalletA _ ->
        case res of
            CreateWalletR (Left _) -> model
            CreateWalletR (Right wallet) -> model { mWallets = wallet : mWallets } -- TODO: use lenses
            _ -> error "This transition should not be reached!"
    GetWalletsA -> model

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions _ ResetWalletA ResetWalletR                          = Top
postconditions _ (CreateWalletA _) (CreateWalletR (Left _)) = Bot
postconditions Model{..} (CreateWalletA _) (CreateWalletR (Right V1.Wallet{..})) = Predicate $ NotElem walId (map V1.walId mWallets)
postconditions Model{..} GetWalletsA (GetWalletsR wallets) =
    -- TODO: bellow won't work because for some reason walSpendingPasswordLastUpdate changes after wallet has been created
    --
    -- ```
    -- sort (DB.toList wallets) .== sort mWallets
    -- ```
    -- TODO: use IxSet here?
    -- sort (map V1.walId $ DB.toList wallets) .== sort (map V1.walId mWallets) -- TODO: this is too weak
    Bot
postconditions _ _ _ =  error "This postcondition should not be reached!"

------------------------------------------------------------------------

-- Action generator

-- TODO: reuse the one from wallet-new/test/unit/Test/Spec/Wallets.hs
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
generator (Model _ False) = pure ResetWalletA
generator Model{..} = frequency
    [ (1, pure ResetWalletA)
    , (5, CreateWalletA . WL.CreateWallet <$> genNewWalletRq)
    , (5, pure GetWalletsA)
    ]

shrinker :: Action Symbolic -> [Action Symbolic]
shrinker _ = []

-- ------------------------------------------------------------------------
--
semantics :: WL.PassiveWalletLayer IO -> PassiveWallet -> Action Concrete -> IO (Response Concrete)
semantics pwl _ cmd = case cmd of
    ResetWalletA -> do
        WL.resetWalletState pwl
        return ResetWalletR
    CreateWalletA cw -> CreateWalletR <$> WL.createWallet pwl cw
    GetWalletsA      -> GetWalletsR <$> WL.getWallets pwl

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
mock _ (CreateWalletA _) = pure $ CreateWalletR (Left $ WL.CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)
mock Model{..} GetWalletsA = pure $ GetWalletsR $ DB.fromList mWallets

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
    cw <- CreateWalletA . WL.CreateWallet <$> generate genNewWalletRq
    let cmds = Commands
            [ Command ResetWalletA mempty
            , Command cw mempty
            , Command GetWalletsA mempty
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
