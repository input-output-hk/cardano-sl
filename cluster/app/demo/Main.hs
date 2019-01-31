{-| Demo cluster nodes. See cluster/README.md -}

{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum hiding (keys)

import           Control.Concurrent.Async (waitAny)
import           Data.Map.Strict ((!))
import           Data.Maybe (fromJust)
import           Formatting (build, sformat, (%))
import           System.Console.ANSI (clearFromCursorToLineEnd,
                     hSetCursorColumn)
import           System.Console.Docopt (Arguments, Docopt, docopt,
                     exitWithUsage, getArg, isPresent, longOption,
                     parseArgsOrExit)
import           System.IO (BufferMode (..), hSetBuffering, stdout)

import           Cardano.Cluster (MaxWaitingTime (..), NodeName (..),
                     NodeType (..), RunningNode (..), mkNamedNodes,
                     startCluster, waitForNode)
import           Cardano.Cluster.Util (ntwrkAddrToBaseUrl, unsafeIntFromString,
                     unsafeNetworkAddressFromString)
import           Cardano.Node.Client (mkHttpClient)
import           Pos.Node.API (SyncPercentage)


-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-sl-cluster-demo

Spawn a demo cluster of nodes running cardano-sl, ready-to-use

Usage:
  cardano-sl-cluster-demo [options]
  cardano-sl-cluster-demo --help

Options:
  --cores=INT   Number of core nodes to start [default: 4]
  --relays=INT  Number of relay nodes to start [default: 1]
  --edges=INT   Number of edge nodes to start [default: 1]
|]


-- | Cluster configuration can be tweaked via ENV vars.
-- Each ENV var is prefixed with the following.
--
-- (e.g. `DEMO_NO_CLIENT_AUTH=True`)
prefix :: String
prefix = "DEMO_"


main :: IO ()
main = void $ do
    hSetBuffering stdout NoBuffering -- Instead of LineBuffering

    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli

    let nCores  = getArgInt args "cores"
    let nRelays = getArgInt args "relays"
    let nEdges  = getArgInt args "edges"

    putTextLn $ sformat
        ("Cluster is starting ("%build%" core(s), "%build%" relay(s), "%build%" edge(s))...")
        nCores nRelays nEdges
    cluster <- startCluster prefix $ mconcat
        [ mkNamedNodes NodeCore nCores
        , mkNamedNodes NodeRelay nRelays
        , mkNamedNodes NodeEdge nEdges
        ]

    handles <- forM cluster $ \case
        RunningNode nodeType (NodeName nodeId) env manager handle -> do
            let addr = unsafeNetworkAddressFromString (env ! "NODE_API_ADDRESS")
            let client = mkHttpClient (ntwrkAddrToBaseUrl addr) manager
            putText "..." >> waitForNode client (MaxWaitingTime 90) printProgress
            putTextFromStart $ "..." <> nodeId <> " OK!"
            when (nodeType /= NodeEdge) $ putText
                $  "\n......address:       " <> toText (env ! "LISTEN")
            -- todo, dont mapm over chars
            putTextLn
                $  "\n......api address:   " <> toText (env ! "NODE_API_ADDRESS")
                <> "\n......doc address:   " <> toText (env ! "NODE_DOC_ADDRESS")
                <> "\n......system start:  " <> toText (env ! "SYSTEM_START")
            return handle

    putTextLn "Cluster is ready!"

    waitAny handles
  where
    -- | Args are defaulted to something, so we know they exist
    getArgInt :: Arguments -> String -> Int
    getArgInt args =
        unsafeIntFromString . fromJust . getArg args . longOption

    putTextFromStart :: Text -> IO ()
    putTextFromStart txt = do
        hSetCursorColumn stdout 0 >> clearFromCursorToLineEnd
        putText txt

    printProgress :: Maybe SyncPercentage -> IO ()
    printProgress progress = do
        case progress of
            Nothing ->
                putTextFromStart "...starting"
            Just p ->
                putTextFromStart $ "...syncing " <> (sformat build p)
