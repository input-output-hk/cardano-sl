module Main where

import           Universum

import           Cardano.Wallet.Demo (mkWHttpClient, startCluster, waitForNode)
import           Control.Concurrent.Async (waitAny)


main :: IO ()
main = do
    let prefix = "DEMO_"

    cluster <- startCluster prefix [ "node0", "node1", "node2" ]
    wc <- mkWHttpClient prefix

    putText "\nWaiting for cluster to start...\n" >> waitForNode wc >> putText "Cluster ready!\n"

    void $ waitAny cluster
