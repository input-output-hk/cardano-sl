module Test.Spec.Submission (
    spec
  ) where

import           Universum

import           Util.Buildable.Hspec
--import           Util.Buildable.QuickCheck

{-------------------------------------------------------------------------------
  Wallet worker state machine tests
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Test wallet submission layer" $ do

      it "supports addition of pending transactions" $ do
          True `shouldBe` False

      it "supports deletion of pending transactions" $ do
          True `shouldBe` False

      it "can schedule transactions for submission" $ do
          True `shouldBe` False

      it "can submit transactions" $ do
          True `shouldBe` False
