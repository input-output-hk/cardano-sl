
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                        (Spec, describe, it, shouldBe)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Socket"

spec :: Spec
spec =
    describe "Methods"
        getTxInfoSpec


getTxInfoSpec :: Spec
getTxInfoSpec =
    describe "getTxInfo" $
        it "has to be tested" $
            True `shouldBe` True
