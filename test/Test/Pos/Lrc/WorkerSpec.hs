-- | Specification of 'Pos.Lrc.Worker' (actually only
-- 'lrcSingleShotNoLock' which probably shouldn't be there, but it
-- doesn't matter now).

module Test.Pos.Lrc.WorkerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (Arbitrary (..), Gen)

import           Test.Pos.Block.Logic.Mode (BlockProperty, TestParams (..),
                                            blockPropertyToProperty)

spec :: Spec
-- Currently we want to run it only once, because there is no much
-- randomization (its effect is likely negligible) and performance is
-- the issue.
spec = describe "Block.Logic.VAR" $ modifyMaxSuccess (const 1) $ do
    describe "lrcSingleShotNoLock" $ do
        prop lrcCorrectnessDesc $
            blockPropertyToProperty genTestParams lrcCorrectnessProp
  where
    lrcCorrectnessDesc =
        "Computes richmen correctly according to the stake distribution " <>
        "right before the '8 * k'-th slot.\n" <>
        "Computes leaders using follow-the-satoshi algorithm using stake " <>
        "distribution or utxo right before the '8 * k'-th slot."

genTestParams :: Gen TestParams
genTestParams = arbitrary

lrcCorrectnessProp :: BlockProperty ()
lrcCorrectnessProp = pass
