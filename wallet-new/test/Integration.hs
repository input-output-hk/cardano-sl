module Main where

import           Universum

import           Test.Hspec (describe, hspec)

import           Integration.Clients (WWebModeRunner (..), mkWHttpClient,
                     mkWWebModeRunner)
import           Integration.Fixtures (generateInitialState)

import qualified Integration.Specs.Addresses as Addresses


main :: IO ()
main = do
    -- The following runners are instantiated using a bunch of ENV variables
    -- that can be used to tweak their behavior.
    --
    -- Note that we don't define a CLI here for multiple reasons:
    --
    --  - It's harder to use with `stack` and other tooling
    --  - It conflicts with `hspec` which already accepts test-arguments
    --
    -- We've defined sensible defaults for all environment variables such that,
    -- in theory one can simply run stack without any arguments with a default
    -- demo cluster running and it will work as expected.
    WWebModeRunner{..} <- mkWWebModeRunner
    wc <- mkWHttpClient

    runWWebMode generateInitialState

    hspec $ describe "Integration Tests" $
        describe "Addresses" $ Addresses.spec wc
