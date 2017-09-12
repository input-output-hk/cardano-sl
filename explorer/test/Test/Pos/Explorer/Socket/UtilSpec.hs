
-- | Tests of Pos.Explorer.Socket.Util

module Test.Pos.Explorer.Socket.UtilSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, it, shouldBe)


----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "Util" $
        describe "regroupBySnd" $
            it "has to be tested" $
                True `shouldBe` True
