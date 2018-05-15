{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.Submission (
    spec
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), genPending, pendingTransactions)
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Submission
import qualified Data.Map as M
import           Data.Text.Buildable (build)
import           Formatting (bprint, (%))
import qualified Formatting as F
import qualified Pos.Core as Core

import           Test.QuickCheck (Gen, Property, forAll, (===))
import           Test.QuickCheck.Monadic (assert, forAllM, monadicIO, run)
import           Util.Buildable (ShowThroughBuild (..))
import           Util.Buildable.Hspec

{-------------------------------------------------------------------------------
  Wallet worker state machine tests
-------------------------------------------------------------------------------}

instance (Buildable a, Buildable b) => Buildable (a,b) where
    build (a,b) = bprint ("(" % F.build % "," % F.build % ")") a b

-- | A mock 'WalletDiffusion' which always reply with either 'True' or 'False'
-- when sending a new 'TxAux' in the ether.
constantWalletDiffusion :: Bool -> WalletDiffusion
constantWalletDiffusion reply = WalletDiffusion {
      walletSendTx = \_ -> return reply
    }

-- | Checks that all the input 'Pending' shows up in the local pending set of
-- the given 'WalletSubmission'.
containsPending :: Pending -> WalletSubmission -> Bool
containsPending p ws =
    let pending      = p ^. pendingTransactions . fromDb
        localPending = ws ^. wsState . wssPendingSet . pendingTransactions . fromDb
    in pending `M.isSubmapOf` localPending

-- | Checks that @any@ of the input transactions (in the pending set) appears
-- in the local pending set of the given 'WalletSubmission'.
doesNotContainPending :: Pending -> WalletSubmission -> Bool
doesNotContainPending p ws =
    let pending      = p ^. pendingTransactions . fromDb
        localPending = ws ^. wsState . wssPendingSet . pendingTransactions . fromDb
    in M.intersection localPending pending == mempty


samePending :: Pending -> WalletSubmission -> Property
samePending p ws = (STB (ws ^. wsState . wssPendingSet)) === (STB p)

genSimpleWalletSubmission :: Gen (ShowThroughBuild WalletSubmission)
genSimpleWalletSubmission =
    STB <$> genWalletSubmission (constantWalletDiffusion True) constantResubmission

genSimplePair :: Gen (ShowThroughBuild (WalletSubmission, Pending))
genSimplePair = do
    pair <- (,) <$> (fmap unSTB genSimpleWalletSubmission)
                <*> genPending (Core.ProtocolMagic 0)
    pure (STB pair)

unsafeScheduleFrom :: WalletSubmission
                   -> Pending
                   -> Schedule
                   -> WalletSubmission
unsafeScheduleFrom ws p customSchedule =
    M.foldlWithKey' updateFn ws (p ^. pendingTransactions . fromDb)
    where
        updateFn :: WalletSubmission
                 -> Core.TxId
                 -> Core.TxAux
                 -> WalletSubmission
        updateFn acc txId txAux = unsafeSchedule acc txId txAux customSchedule

dropImmediately :: Schedule
dropImmediately = Schedule (0, 0)

spec :: Spec
spec = do
    describe "Test wallet submission layer" $ do

      it "supports addition of pending transactions" $
          forAll genSimplePair $ \(unSTB -> (submission, toAdd)) ->
              containsPending toAdd (addPending submission toAdd)

      it "supports deletion of pending transactions" $
          forAll genSimplePair $ \(unSTB -> (submission, toRemove)) ->
              doesNotContainPending toRemove $ remPending submission toRemove

      it "remPending . addPending = id" $
          forAll genSimplePair $ \(unSTB -> (submission, pending)) ->
              let originallyPending = submission ^. wsState ^. wssPendingSet
                  currentlyPending  = remPending (addPending submission pending) pending
              in samePending originallyPending currentlyPending

      it "can drop transactions with TTL <= 0" $ monadicIO $ do
          forAllM genSimplePair $ \(unSTB -> (submission, pending)) -> do
              let submission' = unsafeScheduleFrom submission pending dropImmediately
              dropped <- fst <$> run (tick submission' 42)
              assert (dropped == pending)
