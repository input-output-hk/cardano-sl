{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Universum hiding (init, keys)

import           Control.Lens (at)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Formatting (build, sformat, (%))
import           System.Console.Docopt (Arguments, Docopt, argument, docopt,
                     exitWithUsage, getArg, isPresent, longOption,
                     parseArgsOrExit)
import           System.Environment (getEnvironment)

import           Cardano.Cluster (NodeType (..), mkNamedNodes)
import           Cardano.Cluster.Environment (Artifact, initializeArtifact,
                     prepareEnvironment, withStateDirectory)
import           Cardano.Cluster.Util (stripFilterPrefix, unsafeIntFromString)


-- | Command-Line Interface specification. See http://docopt.org/
cli :: Docopt
cli = [docopt|
cardano-sl-prepare-environment

Generate a default environment for a given cluster configuration

Usage:
  cardano-sl-prepare-environment <prefix> [options]
  cardano-sl-prepare-environment --help

Options:
  --cores=INT   Number of core nodes to start [default: 4]
  --relays=INT  Number of relay nodes to start [default: 1]
  --edges=INT   Number of edge nodes to start [default: 1]
|]


main :: IO ()
main = do
    args <- parseArgsOrExit cli =<< getArgs
    when (args `isPresent` (longOption "help")) $ exitWithUsage cli

    let nCores  = getOptInt args "cores"
    let nRelays = getOptInt args "relays"
    let nEdges  = getOptInt args "edges"
    let prefix = getArgString args "prefix"
    let nodes = mconcat
            [ mkNamedNodes NodeCore nCores
            , mkNamedNodes NodeRelay nRelays
            , mkNamedNodes NodeEdge nEdges
            ]

    putTextLn $ sformat
        ("Generating environment for ("%build%" core(s), "%build%" relay(s), "%build%" edge(s))...")
        nCores nRelays nEdges

    env0 <- (Map.fromList . stripFilterPrefix prefix) <$> getEnvironment
    withStateDirectory (env0 ^. at "STATE_DIR") $ \stateDir -> do
        forM_ nodes $ \node@(_, nodeType) -> do
            let (artifacts, _) = prepareEnvironment node nodes stateDir env0
            let (genesis, topology, logger, tls) = artifacts

            case nodeType of
                NodeCore -> do
                    void (init genesis >> init topology >> init logger >> init tls)

                NodeRelay -> do
                    void (init topology >> init logger >> init tls)

                NodeEdge -> do
                    void (init topology >> init logger >> init tls)
        putTextLn $ sformat
            ("Environment generated in: "%build)
            stateDir
  where
    init :: Artifact a b -> IO b
    init = initializeArtifact

    -- | Args are defaulted to something, so we know they exist
    getOptInt :: Arguments -> String -> Int
    getOptInt args =
        unsafeIntFromString . fromJust . getArg args . longOption

    getArgString :: Arguments -> String -> String
    getArgString args =
        fromJust . getArg args . argument
