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

constantResubmit :: Bool -> ResubmissionFunction Identity
constantResubmit success currentSlot scheduled oldScheduler =
    let send _ = return success
    in defaultResubmitFunction send constantRetry currentSlot scheduled oldScheduler

-- | Checks that all the input 'Pending' shows up in the local pending set of
-- the given 'WalletSubmission'.
containsPending :: Pending -> WalletSubmission m -> Bool
containsPending pending ws =
    let localPending = ws ^. localPendingSet
    in localPending `shouldContainPending` pending

-- | Checks whether or not the second input is fully contained within the first.
shouldContainPending :: Pending -> Pending -> Bool
shouldContainPending p1 p2 =
    let pending1 = p1 ^. pendingTransactions . fromDb
        pending2 = p2 ^. pendingTransactions . fromDb
    in pending2 `M.isSubmapOf` pending1

-- | Checks that @any@ of the input transactions (in the pending set) appears
-- in the local pending set of the given 'WalletSubmission'.
doesNotContainPending :: Pending -> WalletSubmission m -> Bool
doesNotContainPending p ws =
    let pending      = p ^. pendingTransactions . fromDb
        localPending = ws ^. localPendingSet . pendingTransactions . fromDb
    in M.intersection localPending pending == mempty


samePending :: Pending -> WalletSubmission m -> Property
samePending p ws = (STB (ws ^. localPendingSet)) === (STB p)

---
--- Effectul generators, running in IO
---
genWalletSubmissionIO :: Gen (ShowThroughBuild (WalletSubmission IO))
genWalletSubmissionIO =
    STB <$> genWalletSubmission (constantResubmitIO (constantWalletDiffusion True))

genPairIO :: Gen (ShowThroughBuild (WalletSubmission IO, Pending))
genPairIO = do
    pair <- (,) <$> (fmap unSTB genWalletSubmissionIO)
                <*> genPending (Core.ProtocolMagic 0)
    pure (STB pair)

---
--- Pure generators, running in Identity
---
genPureWalletSubmission :: Bool -> Gen (ShowThroughBuild (WalletSubmission Identity))
genPureWalletSubmission success =
    STB <$> genWalletSubmission (constantResubmit success)

genPurePair :: Bool -> Gen (ShowThroughBuild (WalletSubmission Identity, Pending))
genPurePair success = do
    pair <- (,) <$> (fmap unSTB (genPureWalletSubmission success))
                <*> genPending (Core.ProtocolMagic 0)
    pure (STB pair)


unsafeScheduleFrom :: forall m. WalletSubmission m
                   -> Slot
                   -> Pending
                   -> Schedule
                   -> WalletSubmission m
unsafeScheduleFrom ws slot p customSchedule =
    M.foldlWithKey' updateFn ws (p ^. pendingTransactions . fromDb)
    where
        updateFn :: WalletSubmission m
                 -> Core.TxId
                 -> Core.TxAux
                 -> WalletSubmission m
        updateFn acc txId txAux = unsafeSchedule acc slot (Scheduled (txId, txAux, customSchedule))

dropImmediately :: Schedule
dropImmediately = Schedule 255

spec :: Spec
spec = do
    describe "Test wallet submission layer" $ do

      it "supports addition of pending transactions" $
          forAll (genPurePair True) $ \(unSTB -> (submission, toAdd)) ->
              containsPending toAdd (addPending submission toAdd)

      it "supports deletion of pending transactions" $
          forAll (genPurePair True) $ \(unSTB -> (submission, toRemove)) ->
              doesNotContainPending toRemove $ remPending submission toRemove

      it "remPending . addPending = id" $
          forAll (genPurePair True) $ \(unSTB -> (submission, pending)) ->
              let originallyPending = submission ^. localPendingSet
                  currentlyPending  = remPending (addPending submission pending) pending
              in samePending originallyPending currentlyPending

      it "increases its internal slot after ticking" $ do
          forAll (genPureWalletSubmission True) $ \(unSTB -> submission) ->
              let slotNow = submission ^. getCurrentSlot
                  (_, ws') = runIdentity $ tick submission
                  in ws' ^. getCurrentSlot === slotNow + 1

      it "can drop transactions which can be successfully transmitted" $ do
          forAll (genPurePair True) $ \(unSTB -> (submission, pending)) ->
              let slot = submission ^. getCurrentSlot
                  submission' = unsafeScheduleFrom submission slot pending (Schedule 0)
                  dropped = fst . runIdentity . tick $ submission'
              in dropped `shouldContainPending` pending

      it "can drop transactions which exceeded the resubmission count" $ do
          forAll (genPurePair False) $ \(unSTB -> (submission, pending)) ->
              let slot = submission ^. getCurrentSlot
                  submission' = unsafeScheduleFrom submission slot pending dropImmediately
                  dropped = fst . runIdentity . tick $ submission'
              in dropped `shouldContainPending` pending

      it "constantResubmitIO works as intended in the happy path scenario" $ monadicIO $ do
          forAllM genPairIO $ \(unSTB -> (submission, pending)) -> do
              let slot = submission ^. getCurrentSlot
                  submission' = unsafeScheduleFrom submission slot pending dropImmediately
              dropped <- fst <$> run (tick submission')
              assert (dropped `shouldContainPending` pending)
