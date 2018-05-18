{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.Submission (
    spec
  ) where

import           Universum

import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Pending (..), emptyPending, genPending,
                                                pendingTransactions, singletonPending, unionPending)
import           Cardano.Wallet.Kernel.Submission
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Text.Buildable (build)
import           Formatting (bprint, (%))
import qualified Formatting as F
import qualified Pos.Core as Core

import           Test.QuickCheck (Gen, Property, forAll, (===), (==>))
import           Util.Buildable (ShowThroughBuild (..))
import           Util.Buildable.Hspec

{-------------------------------------------------------------------------------
  Submission layer tests
-------------------------------------------------------------------------------}

instance (Buildable a, Buildable b) => Buildable (a,b) where
    build (a,b) = bprint ("(" % F.build % "," % F.build % ")") a b

constantResubmit :: Bool -> ResubmissionFunction Identity
constantResubmit success currentSlot scheduled oldScheduler =
    let send _  = return success
        rPolicy = constantRetry 255
    in defaultResubmitFunction send rPolicy currentSlot scheduled oldScheduler

giveUpAfter :: Int -> ResubmissionFunction Identity
giveUpAfter retries currentSlot scheduled oldScheduler =
    let send _  = return False
        rPolicy = constantRetry retries
    in defaultResubmitFunction send rPolicy currentSlot scheduled oldScheduler

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

shouldNotContainPending :: Pending -> Pending -> Bool
shouldNotContainPending p1 p2 = not (shouldContainPending p1 p2)

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

unsafeScheduleFrom :: forall m. Slot
                   -> Pending
                   -> SubmissionCount
                   -> WalletSubmission m
                   -> WalletSubmission m
unsafeScheduleFrom slot p retries ws =
    M.foldlWithKey' updateFn ws (p ^. pendingTransactions . fromDb)
    where
        updateFn :: WalletSubmission m
                 -> Core.TxId
                 -> Core.TxAux
                 -> WalletSubmission m
        updateFn acc txId txAux =
            addPending (singletonPending txId txAux)
                       (overrideSchedule acc slot (ScheduledTx txId txAux retries))

dropImmediately :: SubmissionCount
dropImmediately = SubmissionCount 255

spec :: Spec
spec = do
    describe "Test wallet submission layer" $ do

      it "supports addition of pending transactions" $
          forAll (genPurePair True) $ \(unSTB -> (submission, toAdd)) ->
              containsPending toAdd (addPending toAdd submission)

      it "supports deletion of pending transactions" $
          forAll (genPurePair True) $ \(unSTB -> (submission, toRemove)) ->
              doesNotContainPending toRemove $ remPending toRemove submission

      it "remPending . addPending = id" $
          forAll (genPurePair True) $ \(unSTB -> (submission, pending)) ->
              let originallyPending = submission ^. localPendingSet
                  currentlyPending  = remPending pending (addPending pending submission)
              in samePending originallyPending currentlyPending

      it "increases its internal slot after ticking" $ do
          forAll (genPureWalletSubmission True) $ \(unSTB -> submission) ->
              let slotNow = submission ^. getCurrentSlot
                  (_, ws') = runIdentity $ tick submission
                  in ws' ^. getCurrentSlot === mapSlot succ slotNow

      it "limit retries correctly" $ do
          forAll (genPurePair True) $ \(unSTB -> (ws, pending)) ->
              pending /= emptyPending ==>
                  let ws' = unsafeScheduleFrom (ws ^. getCurrentSlot)
                                               pending
                                               (SubmissionCount 0)
                                               (ws & wsResubmissionFunction .~ giveUpAfter 5)
                      updateFn (d, newWs) _ = bimap (unionPending d) identity (runIdentity (tick newWs))
                      (dropped, _) = List.foldl' updateFn (emptyPending, ws') ([1..6] :: [Int])
                      in dropped `shouldContainPending` pending

      -- The evicted set will never contain transactions successfully retransmitted.
      -- It's not this layer's responsibility to keep the pending set consistent, as
      -- the single source of truth for the pending set is the blockchain and the BListener.
      it "won't drop transactions which has been successfully transmitted" $ do
          forAll (genPurePair True) $ \(unSTB -> (submission, pending)) ->
              pending /= emptyPending ==>
                  let slot = submission ^. getCurrentSlot
                      submission' = unsafeScheduleFrom slot pending (SubmissionCount 0) submission
                      dropped = fst . runIdentity . tick $ submission'
                  in dropped `shouldNotContainPending` pending

      it "can drop transactions which exceeded the resubmission count" $ do
          forAll (genPurePair False) $ \(unSTB -> (submission, pending)) ->
              let slot = submission ^. getCurrentSlot
                  submission' = unsafeScheduleFrom slot pending dropImmediately submission
                  dropped = fst . runIdentity . tick $ submission'
              in dropped `shouldContainPending` pending
