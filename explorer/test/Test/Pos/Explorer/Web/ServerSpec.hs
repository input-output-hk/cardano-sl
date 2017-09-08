-- | This module is an example for tests.

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec              (Spec, describe, it, shouldBe)
--import           Test.Hspec.QuickCheck (prop)
--
--import           Pos.Explorer.Web.Server (getStatsTxs)

spec :: Spec
spec = describe "Test" $ do
    it "should be equal" $
        True `shouldBe` True
