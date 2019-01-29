module Main where

import           Universum hiding (keys)

import           Data.Map.Strict ((!))
import           Test.Hspec (beforeAll, describe, hspec)

import           Cardano.Cluster (MaxWaitingTime (..), NodeType (..))
import           Cardano.Cluster.Util (ntwrkAddrToBaseUrl,
                     unsafeNetworkAddressFromString)
import           Cardano.Wallet.Client.Http (mkHttpClient, mkHttpDocClient)
import qualified Test.Integration.Documentation as Documentation
import           Test.Integration.Framework.Cluster (startCluster, waitForNode)
import           Test.Integration.Framework.DSL (Context (..))
import qualified Test.Integration.Scenario.Accounts as Accounts
import qualified Test.Integration.Scenario.Addresses as Addresses
import qualified Test.Integration.Scenario.Node as Node
import qualified Test.Integration.Scenario.Transactions as Transactions
import qualified Test.Integration.Scenario.Wallets as Wallets

main :: IO ()
main = do
    -- Start cluster
    putTextLn "Starting cluster of nodes... ╰( ͡° ͜ʖ ͡° )つ──☆*:・ﾟ"
    putTextLn "Suggestion: tail -F ./state-integration/logs/edge.log.pub"
    (env, keys, manager) <- startCluster
        [ ("core0", NodeCore)
        , ("core1", NodeCore)
        , ("core2", NodeCore)
        , ("relay", NodeRelay)
        , ("edge", NodeEdge)
        ]
    let wAddr   = toBaseUrl $ env ! "WALLET_ADDRESS"
    let dAddr   = toBaseUrl $ env ! "WALLET_DOC_ADDRESS"
    let wClient = mkHttpClient wAddr manager
    let dClient = mkHttpDocClient dAddr manager
    waitForNode wClient (MaxWaitingTime 90)

    -- Run tests
    hspec $ do
        describe "API Documentation" $ Documentation.spec dClient

        beforeAll (newMVar $ Context keys wClient (wAddr, manager)) $ do
            describe "Accounts" Accounts.spec
            describe "Addresses" Addresses.spec
            describe "Transactions" Transactions.spec
            describe "Wallets" Wallets.spec
            describe "Node" Node.spec
  where
    toBaseUrl = ntwrkAddrToBaseUrl . unsafeNetworkAddressFromString
