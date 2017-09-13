
-- | Tests of Pos.Explorer.Socket.Methods

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Socket.MethodsSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                        (Spec, describe, it, shouldThrow, anyException)

import           Pos.Explorer.Socket.Methods       (fromCAddressOrThrow)
import           Pos.Explorer.Web.ClientTypes      (CAddress(..))

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
    describe "fromCAddressOrThrow" $
        it "throws an exception if a given CAddress is invalid" $
            fromCAddressOrThrow (CAddress "invalid" ) `shouldThrow` anyException
