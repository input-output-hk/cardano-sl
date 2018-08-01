{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum

import           NeatInterpolation (text)
import           Test.Hspec (describe, hspec)

import           Integration.Clients (WWebModeRunner (..), mkWHttpClient,
                     mkWWebModeRunner)
import           Integration.Fixtures (generateInitialState)

import qualified Integration.Specs.Addresses as Addresses


import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Options.Applicative (info)
import           System.Environment (lookupEnv, setEnv)
import           System.Wlog (LoggerName (..))

import           Cardano.Wallet.Launcher (startCoreNode, startEdgeNode)
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     WalletStartupOptions (..), execParserEnv,
                     walletBackendParamsParser)
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)

data NodeType = CoreNode | EdgeNode

main :: IO ()
main = putText [text|
========== INTEGRATION TESTS ==========
Integration tests run using a local cluster of 3 core nodes + 1 wallet node.
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

    - node-0: localhost@3000
    - node-1: localhost@3001
    - node-2: localhost@3002
    - node-edge: localhost@3003

|] >> do
    wc <- mkWHttpClient

    -- NOTE Always EdgeNode in the end as its options conflicts with the core nodes.
    -- Also, there should be only one edge node. In the end, this would deserve
    -- a better data-structure I guess. For this sake, a list is fine,
    _ <- startCluster
        [ ("node-0", CoreNode)
        , ("node-1", CoreNode)
        , ("node-2", CoreNode)
        , ("node-edge", EdgeNode)
        ]

    putText "Waiting for cluster to start..."

    threadDelay 1000000000000

    hspec $ describe "Integration Tests" $
        describe "Addresses" $ Addresses.spec wc

  where
    as :: String -> String -> IO ()
    as var def =
        setEnv var =<< (fromMaybe def <$> lookupEnv var)

    startCluster :: [(String, NodeType)] -> IO [ThreadId]
    startCluster nodes = do
        let prefix = "INTEGRATION_TESTS_"

        (prefix <> "DB_PATH")    `as` "state-integration-tests/db"
        (prefix <> "REBUILD_DB") `as` "True"
        (prefix <> "LISTEN")     `as` "3000"
        (prefix <> "TOPOLOGY")   `as` "state-integration-tests/topology.yaml"
        (prefix <> "TLSCERT")    `as` "state-integration-tests/tls/server.crt"
        (prefix <> "TLSKEY")     `as` "state-integration-tests/tls/server.key"
        (prefix <> "TLSCA")      `as` "state-integration-tests/tls/ca.crt"

        forM nodes $ \(nodeId, nodeType) -> do
            let lName  = LoggerName (toText nodeId)

            (prefix <> "NODE_ID")    `as` nodeId
            (prefix <> "DB_PATH")    `as` ("state-integration-tests/db/" <> nodeId)
            (prefix <> "LOG_CONFIG") `as` ("state-integration-tests/logs/" <> nodeId <> ".yaml")

            cArgs <- execParserEnv prefix (info commonNodeArgsParser mempty)
            nArgs <- execParserEnv prefix (info nodeArgsParser mempty)

            case nodeType of
                CoreNode ->
                    forkIO $ startCoreNode cArgs nArgs lName

                EdgeNode -> do
                    (prefix <> "WALLET_ADDRESS")    `as` "127.0.0.1:8090"
                    (prefix <> "WALLET_REBUILD_DB") `as` "True"
                    (prefix <> "WALLET_DB_PATH")    `as` "state-integration-tests/wallet-db"

                    wArgs <- execParserEnv prefix (info walletBackendParamsParser mempty)
                    let wOpts = WalletStartupOptions cArgs (WalletLegacy wArgs)

                    -- runWWebMode cArgs nArgs wArgs lName generateInitialState

                    forkIO $ startEdgeNode nArgs wOpts lName
