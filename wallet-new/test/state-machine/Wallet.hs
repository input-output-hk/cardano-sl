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
--    | GetWalletA V1.WalletId
--    | UpdateWalletA V1.WalletId V1.WalletUpdate
    deriving (Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

data Response (r :: * -> *)
    = ResetWalletR
    | CreateWalletR (Either WL.CreateWalletError V1.Wallet)
    | GetWalletsR (DB.IxSet V1.Wallet)
--    | GetWalletR (Either WL.GetWalletError V1.Wallet)
--    | UpdateWalletR (Either WL.UpdateWalletError V1.Wallet)
    deriving (Show, Generic1, Rank2.Foldable)


------------------------------------------------------------------------

-- Wallet state

-- TODO: should we put PassiveWallet reference into the model?
-- this is how CircurlarBufer.hs does it - it saves a spec and uses
-- a spec to check real state
data Model (r :: * -> *) = Model
    { mWallets :: [V1.Wallet] -- consider using IxSet
    , mReset   :: Bool
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
-- preconditions _ (GetWalletA _)      = Top
-- preconditions _ (UpdateWalletA _ _) = Top

transitions :: Model r -> Action r -> Response r -> Model r
transitions model@Model{..} cmd res = case cmd of
    ResetWalletA -> Model [] True
    CreateWalletA _ ->
        case res of
            CreateWalletR (Left _) -> model
            CreateWalletR (Right wallet) -> model { mWallets = wallet : mWallets } -- TODO: use lenses
            _ -> error "This transition should not be reached!"
    -- TODO: handle monadic exception?
    GetWalletsA -> model
--     GetWalletA _ -> model
--     UpdateWalletA wId V1.WalletUpdate{..} ->
--         let thisWallet = (wId ==) . V1.walId
--             updatedWallets = map update $ filter thisWallet mWallets
--             update w = w { V1.walAssuranceLevel = uwalAssuranceLevel, V1.walName = uwalName }
--         in model { mWallets = updatedWallets <> filter (not . thisWallet) mWallets }

postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions _ ResetWalletA ResetWalletR                          = Top
 -- TODO: pissibly check that wallet wasn't added
 -- check that ratio of errors is normal/expected

-- NOTE: it should be ok for a wallet creation to fail, but not currently in our tests.
postconditions _ (CreateWalletA _) (CreateWalletR (Left _)) = Bot
-- TODO: check that wallet request and wallet response contain same attributes
postconditions Model{..} (CreateWalletA _) (CreateWalletR (Right V1.Wallet{..})) = Predicate $ NotElem walId (map V1.walId mWallets)
postconditions Model{..} GetWalletsA (GetWalletsR wallets) = sort (DB.toList wallets) .== sort mWallets -- TODO: use IxSet here?
-- postconditions Model{..} (GetWalletA wId) (GetWalletR (Left _)) = Predicate $ NotElem wId (map V1.walId mWallets)
-- -- TODO: also check is returned wallet similar to the one fined in a model
-- postconditions Model{..} (GetWalletA wId) (GetWalletR (Right _)) = Predicate $ Elem wId (map V1.walId mWallets)
-- postconditions Model{..} (UpdateWalletA wId _) (UpdateWalletR (Left _)) = Predicate $ NotElem wId (map V1.walId mWallets)
-- -- TODO: also check does updated wallet contain all relevant info
-- postconditions Model{..} (UpdateWalletA wId _) (UpdateWalletR (Right _)) = Predicate $ Elem wId (map V1.walId mWallets)
-- FIXME: don't catch all errors with this this catch-all match!
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
    -- TODO: add generator for importing wallet from secret key
    , (5, pure GetWalletsA)
    -- This tests fetching existing wallet (except when there is no wallets in model)
--    , (4, GetWalletA . V1.walId <$> oneof (arbitrary:map pure mWallets))
--    -- This tests fetching probably non existing wallet
--    , (1, GetWalletA <$> arbitrary)
--    -- This tests updates existing wallets (except when there is no wallets)
--    , (4, UpdateWalletA . V1.walId <$> oneof (arbitrary:map pure mWallets) <*> arbitrary)
--    -- This tests updating non existing wallet
--    , (1, UpdateWalletA <$> arbitrary <*> arbitrary)
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
    GetWalletsA      -> GetWalletsR <$> WL.getWallets pwl
--    GetWalletA wId -> GetWalletR <$> WL.getWallet pwl wId
--    UpdateWalletA wId update -> UpdateWalletR <$> WL.updateWallet pwl wId update

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

-- NOTE: I (akegalj) was not sure how library exactly uses mock so there is an explanation here https://github.com/advancedtelematic/quickcheck-state-machine/issues/236#issuecomment-431858389
-- NOTE: `mock` is not used in a current quickcheck-state-machine-0.4.2 so in practice we could leave it out. Its still in an experimental phase and there is a possibility it will be added in future versions of this library, so we won't leave it out just yet
mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock _ ResetWalletA      = pure ResetWalletR
-- TODO: add mocking up creating an actual wallet
-- For example you can take one from the model, just change wallet id
mock _ (CreateWalletA _) = pure $ CreateWalletR (Left $ WL.CreateWalletError Kernel.CreateWalletDefaultAddressDerivationFailed)
mock Model{..} GetWalletsA = pure $ GetWalletsR $ DB.fromList mWallets
-- TODO: model other error paths like UnknownHdRoot?
-- mock Model{..} (GetWalletA wId) =
--     let mExists = safeHead $ filter ((wId ==) . V1.walId) mWallets
--         response = maybe (Left $ WL.GetWalletErrorNotFound wId) Right mExists
--     in pure $ GetWalletR response
-- -- TODO: model other error paths?
-- mock Model{..} (UpdateWalletA wId V1.WalletUpdate{..}) =
--     let thisWallet = (wId ==) . V1.walId
--         update w = w { V1.walAssuranceLevel = uwalAssuranceLevel, V1.walName = uwalName }
--         mExists = update <$> safeHead (filter thisWallet mWallets)
--         response = maybe (Left $ WL.UpdateWalletErrorNotFound wId) Right mExists
--     in pure $ UpdateWalletR response


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
    let cmds' = cmds -- Commands [Command ResetWalletA mempty] <> cmds
    print $ commandNamesInOrder cmds'
    (hist, _, res) <- runCommands sm cmds'
    prettyCommands sm hist $
        checkCommandNames cmds' (res === Ok)
  where
    sm = stateMachine pwl pw

prop_walletParallel :: (WL.PassiveWalletLayer IO, PassiveWallet) -> Property
prop_walletParallel (pwl, pw) =
  forAllParallelCommands sm $ \cmds -> monadicIO $
    -- TODO: fix quickcheck with state machine pretty printer output (works well with tasty)
    prettyParallelCommands cmds =<< runParallelCommandsNTimes 100 sm cmds
  where
    sm = stateMachine pwl pw
