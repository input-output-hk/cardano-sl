{ runHaskell, cardanoPkgs }:

runHaskell "yamlValidation" cardanoPkgs (ps: with ps; [
  cardano-sl-util
  cardano-sl-infra
  split
]) {
  logConfigs = [
    ../../log-configs/daedalus.yaml
    #../../log-configs/greppable.yaml
    ../../log-configs/connect-to-cluster.yaml
    ../../log-configs/cluster.yaml
    ../../log-configs/template-demo.yaml
  ];
  topologyConfigs = [
    ../../docs/network/example-topologies/mainnet-staging.yaml
    ../../docs/network/example-topologies/behind-nat-no-dns.yaml
    ../../docs/network/example-topologies/behind-nat-with-dns.yaml
    ../../docs/network/example-topologies/p2p.yaml
    ../../docs/network/example-topologies/static-no-dns.yaml
    ../../docs/network/example-topologies/static-with-dns.yaml
    ../../docs/network/example-topologies/traditional.yaml
  ];
} ''
  import Pos.Util.Log.LoggerConfig (LoggerConfig)
  import Pos.Infra.Network.Yaml (Topology)
  import Data.List.Split
  import System.Environment
  import Data.Yaml
  import Control.Monad.Catch
  import Data.Monoid

  main :: IO ()
  main = do
    runTest "logConfigs" checkLogConfig
    runTest "topologyConfigs" checkTopology
    outpath <- getEnv "out"
    writeFile outpath "done"

  runTest :: FromJSON a => String -> (String -> IO a) -> IO ()
  runTest var func = do
    paths <- getEnv var
    let
      pathList = splitOn " " paths
      doTest path = do
        putStrLn $ "testing: " <> path
        func path
    mapM_ doTest pathList

  checkLogConfig :: String -> IO LoggerConfig
  checkLogConfig path = do
    either throwM return =<< decodeFileEither path

  checkTopology :: String -> IO Topology
  checkTopology path = either throwM return =<< decodeFileEither path
''
