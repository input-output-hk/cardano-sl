-- | This module is an example for tests.

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe, it, shouldBe)
--import           Test.Hspec.QuickCheck (prop)
import           Universum


spec :: Spec
spec = describe "Test" $
    it "should be equal" $ 
        True `shouldBe` True