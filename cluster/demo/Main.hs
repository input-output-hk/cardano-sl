{-| Demo cluster of wallet nodes. See cluster/README.md -}

{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum hiding (keys)

import           Control.Concurrent.Async (waitAny)
import           Data.Map.Strict (lookup, (!))
import           Data.Maybe (fromJust)
import           Formatting (build, sformat, (%))
import           System.Console.ANSI (clearFromCursorToLineEnd,
                     hSetCursorColumn)
import           System.Console.Docopt (Arguments, Docopt, docopt,
                     exitWithUsage, getArg, isPresent, longOption,
                     parseArgsOrExit)
import           System.IO (BufferMode (..), hSetBuffering, stdout)

import           Cardano.Cluster (MaxWaitingTime (..), NodeName (..),
                     NodeType (..), RunningNode (..), startCluster,
                     waitForNode)
import           Cardano.Cluster.Util (unsafeIntFromString)
import           Cardano.Wallet.API.V1.Types (SyncPercentage, WalletImport (..))
import           Cardano.Wallet.Client (WalletClient (..))


-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-sl-cluster-demo

Spawn a demo cluster of nodes running cardano-sl, ready-to-use

Usage:
  cardano-sl-cluster-demo [--no-genesis-wallets] [options]
  cardano-sl-cluster-demo --help

Options:
  --cores=INT          Number of core nodes to start [default: 4]
  --relays=INT         Number of relay nodes to start [default: 1]
  --edges=INT          Number of edge nodes (wallet) to start [default: 1]
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
        RunningCoreNode (NodeName nodeId) env handle -> do
            putTextLn $ "..."
                <> nodeId <> " has no health-check API."
                <> "\n......system start:  "   <> toText (env ! "SYSTEM_START")
                <> "\n......address:       "   <> toText (env ! "LISTEN")
                <> "\n......locked assets: " <> maybe "-" toText ("ASSET_LOCK_FILE" `lookup` env)
            return handle

        RunningRelayNode (NodeName nodeId) env handle -> do
            putTextLn $ "..."
                <> nodeId <> " has no health-check API."
                <> "\n......system start:  " <> toText (env ! "SYSTEM_START")
                <> "\n......address:       " <> toText (env ! "LISTEN")
            return handle

        RunningWalletNode (NodeName nodeId) env client keys handle -> do
            putText "..." >> waitForNode client (MaxWaitingTime 90) printProgress

            unless (args `isPresent` (longOption "no-genesis-wallets")) $ do
                putTextFromStart "...Importing genesis wallets"
                forM_ keys (importWallet client . WalletImport Nothing)

            putTextFromStart $ "..." <> nodeId <> " OK!"
            putTextLn
                $  "\n......system start:  " <> toText (env ! "SYSTEM_START")
                <> "\n......api address:   " <> toText (env ! "WALLET_ADDRESS")
                <> "\n......doc address:   " <> toText (env !  "WALLET_DOC_ADDRESS")
            return handle
    putTextLn "Cluster is (probably) ready!"

    waitAny handles
  where
    -- | Args are defaulted to something, so we know they exist
    getArgInt :: Arguments -> String -> Int
    getArgInt args =
        unsafeIntFromString . fromJust . getArg args . longOption

    -- | Create a list of named nodes of the given type
    mkNamedNodes :: NodeType -> Int -> [(NodeName, NodeType)]
    mkNamedNodes NodeCore 1  = [("core", NodeCore)]
    mkNamedNodes NodeRelay 1 = [("relay", NodeRelay)]
    mkNamedNodes NodeEdge 1  = [("wallet", NodeEdge)]
    mkNamedNodes typ n = zip  (mkIndexedName typ <$> iterate (+1) 0) (replicate n typ)

    -- | Create a @NodeName@ from the given @NodeType@ and index
    mkIndexedName :: NodeType -> Int -> NodeName
    mkIndexedName NodeCore  n = NodeName ("core"   <> show n)
    mkIndexedName NodeRelay n = NodeName ("relay"  <> show n)
    mkIndexedName NodeEdge  n = NodeName ("wallet" <> show n)

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
