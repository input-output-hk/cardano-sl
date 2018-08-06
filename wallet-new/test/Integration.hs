{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum

import           Control.Concurrent (threadDelay)
import           NeatInterpolation (text)
import           Test.Hspec (describe, hspec)

import           Integration.Clients (mkWHttpClient, startCluster)

import qualified Integration.Specs.Addresses as Addresses



main :: IO ()
main = putText [text|
========== INTEGRATION TESTS ==========
Integration tests run using a local cluster of 3 core nodes with wallet enabled.


#### Configuring Nodes

One can tweak a few things using environment variables, in a similar fashion arguments
would be passed to node.

e.g. `--db-path path` ~> `INTEGRATION_TESTS_DB_PATH path`"

By default, the state is dumped in 'wallet-new/state-integration-tests' and
arguments are set in a way that makes integration tests run smoothly. Also, note
that environment variables define "base" arguments for a node. When it makes
sense (.e.g 'DB_PATH', 'LOG_CONFIG'), these "base" arguments are postfixed
with an index (-0, -1, -2 or -edge).

Beside nodes have successive ports which depends on the INTEGRATION_TESTS_LISTEN
environment variable defining port for the first node. For instance, if its
value is set to 3000, nodes will have the following topology:

    - node0: localhost@3000
    - node1: localhost@3001
    - node2: localhost@3003

In a similar fashion, nodes expose their wallet API on the following addresses:

    - node0: localhost@8090
    - node1: localhost@8091
    - node2: localhost@8092

Note that the documentation server listens on the wallet's port plus 100,
e.g. 8190 (resp. 8191 and 8192).

#### Configuring Http Client

The Http client talking to nodes may be configured using 4 ENV variables:

    - INTEGRATION_TESTS_TLSCERT_CLIENT: Path to a client TLS certificate
    - INTEGRATION_TESTS_TLSKEY_CLIENT:  Path to a client TLS Key
    - INTEGRATION_TESTS_TLSCA:          Path to a CA TLS certificate
    - INTEGRATION_TESTS_WALLET_ADDRESS: Network address of the target wallet node

By default, it will use certificates generated in 'wallet-new/state-integration-tests'
and talk to `127.0.0.1:8090'.

|] >> do
    let prefix = "INTEGRATION_TESTS_"

    -- NOTE Always WalletNode in the end as its options conflicts with the core nodes.
    -- Also, there should be only one edge node. In the end, this would deserve
    -- a better data-structure I guess. For this sake, a list is fine,
    cluster <- startCluster prefix [ "node0", "node1", "node2" ]
    wc <- mkWHttpClient prefix


    -- FIXME Try to make that nicer using some sort of MVar lock
    putText "\nWaiting for cluster to start...\n" >> threadDelay 15000000

    hspec $ describe "Integration Tests" $
        describe "Addresses" $ Addresses.spec wc
