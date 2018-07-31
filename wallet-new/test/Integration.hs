module Main where

import           Universum

import           Test.Hspec (describe, hspec)

import           Integration.Clients (WWebModeRunner (..), WalletClient,
                     describeOptions, mkWHttpClient, mkWWebModeRunner)
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

    writeHeader
        [ describeOptions (Proxy @WWebModeRunner)
        , describeOptions (Proxy @WalletClient)
        ]

    WWebModeRunner{..} <- mkWWebModeRunner
    wc <- mkWHttpClient

    runWWebMode generateInitialState

    hspec $ describe "Integration Tests" $
        describe "Addresses" $ Addresses.spec wc

  where
    writeHeader :: [Text] -> IO ()
    writeHeader opts = do
        putText "\n========== INTEGRATION TESTS ==========\n\n"
        putText "Integration tests run using a local cluster of 3 core nodes + 1 wallet node.\n"
        putText "By default, the state is dumped in 'wallet-new/state-integration-tests'.\n"
        putText "One can tweak a few things via some environment variables as follows.\n\n"
        mapM_ putText opts
