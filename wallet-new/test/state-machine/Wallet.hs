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
  , prop_walletParallel
  , withWalletLayer
  )
  where

import           Universum

import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.TreeDiff (ToExpr (toExpr))
import           GHC.Generics (Generic, Generic1)
import           Test.QuickCheck (Gen, Property, arbitrary, frequency, oneof,
                     (===))
import           Test.QuickCheck.Monadic (monadicIO)

import           Test.StateMachine
import           Test.StateMachine.Types (StateMachine)

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
    | GetWalletA V1.WalletId
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    | CreateWalletR (Either WL.CreateWalletError V1.Wallet)
    | GetWalletsR (DB.IxSet V1.Wallet)
    | GetWalletR (Either WL.GetWalletError V1.Wallet)
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

-- TODO: should we put PassiveWallet reference into the model?
-- this is how CircurlarBufer.hs does it - it saves a spec and uses
-- a spec to check real state
data Model (r :: * -> *) = Model
    { mWallets :: [V1.Wallet] -- consider using IxSet
    }
    deriving (Eq, Show, Generic)

-- TODO: can we make this shorter and derive only necessary things
-- FIXME: orphan instances
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
initModel = Model []

preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions _ ResetWalletA      = Top
preconditions _ (CreateWalletA _) = Top
preconditions _ GetWalletsA       = Top
preconditions _ (GetWalletA _)    = Top

transitions :: Model r -> Action r -> Response r -> Model r
transitions model@Model{..} cmd res = case cmd of
    ResetWalletA -> initModel
    CreateWalletA _ ->
        case res of
            CreateWalletR (Left _) -> model
            CreateWalletR (Right wallet) -> model { mWallets = wallet : mWallets } -- TODO: use lenses
            _ -> error "This transition should not be reached!"
    -- TODO: handle monadic exception?
    GetWalletsA -> model
    GetWalletA _ -> model

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions _ ResetWalletA ResetWalletR                          = Top
 -- TODO: pissibly check that wallet wasn't added
 -- check that ratio of errors is normal/expected
postconditions _ (CreateWalletA _) (CreateWalletR (Left _)) = Top
-- TODO: check that wallet request and wallet response contain same attributes
postconditions Model{..} (CreateWalletA _) (CreateWalletR (Right _)) =  Top -- wallet `elem` mWallets
postconditions Model{..} GetWalletsA (GetWalletsR wallets) = DB.fromList mWallets .== wallets
postconditions Model{..} (GetWalletA wId) (GetWalletR (Left _)) = Predicate $ NotElem wId (map V1.walId mWallets)
-- TODO: also check is returned wallet similar to the one fined in a model
postconditions Model{..} (GetWalletA wId) (GetWalletR (Right _)) = Predicate $ Elem wId (map V1.walId mWallets)
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
generator Model{..} = frequency
    [ (1, pure ResetWalletA)
    , (5, CreateWalletA . WL.CreateWallet <$> genNewWalletRq)
    -- TODO: add generator for importing wallet from secret key
    , (5, pure GetWalletsA)
    -- This tests fetching mostly wallets
    , (4, GetWalletA . V1.walId <$> oneof (arbitrary:map pure mWallets))
    -- This tests fetching probably non existing wallet
    , (1, GetWalletA <$> arbitrary)
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
    CreateWalletA cw -> CreateWalletR <$> WL.createWallet pwl cw
    GetWalletsA -> GetWalletsR <$> WL.getWallets pwl
    GetWalletA wId -> GetWalletR <$> WL.getWallet pwl wId

-- TODO: reuse withLayer function defined in wallet-new/test/unit/Test/Spec/Fixture.hs
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

mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _ ResetWalletA      = pure ResetWalletR
-- TODO: add mocking up creating an actual wallet
-- For example you can take one from the model, just change wallet id
mock _ (CreateWalletA _) = pure $ CreateWalletR (Left $ WL.CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)
mock Model{..} GetWalletsA = pure $ GetWalletsR $ DB.fromList mWallets
-- TODO: model other error paths like UnknownHdRoot?
mock Model{..} (GetWalletA wId) =
    let mExists = safeHead $ filter ((wId ==) . V1.walId) mWallets
        response = maybe (Left $ WL.GetWalletErrorNotFound wId) Right mExists
    in pure $ GetWalletR response

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

prop_wallet :: (WL.PassiveWalletLayer IO, PassiveWallet) -> Property
prop_wallet (pwl, pw) = forAllCommands sm Nothing $ \cmds -> monadicIO $ do
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
