{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum

import           NeatInterpolation (text)
import           Test.Hspec (describe, hspec)

import           Cardano.Wallet.Demo (mkWHttpClient, startCluster, waitForNode)

import qualified Integration.Specs.Addresses as Addresses


main :: IO ()
main = putText [text|
========== INTEGRATION TESTS ==========
Integration tests run using a local cluster of 3 core nodes with wallet enabled.

To do so, they rely on `cardano-sl-demo` to setup the environment and the HTTP
client used for testing. These are slightly configurable via ENV var, following
what's explained in `demo/README.md`.

Note that the prefix used here is `INTEGRATION_TESTS_` and that everything
related to the execution is stored in `wallet-new/state-integration-tests`. For
example, logs are accessible in this folder.

|] >> do
    let prefix = "INTEGRATION_TESTS_"

    -- NOTE Always WalletNode in the end as its options conflicts with the core nodes.
    -- Also, there should be only one edge node. In the end, this would deserve
    -- a better data-structure I guess. For this sake, a list is fine,
    wc <- startCluster prefix [ "node0", "node1", "node2" ] >> mkWHttpClient prefix

    putText "\nWaiting for cluster to start...\n" >> waitForNode wc

    hspec $ describe "Integration Tests" $
        describe "Addresses" $ Addresses.spec wc
