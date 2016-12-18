{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module NodeOptions
       ( Args (..)
       , getNodeOptions
       ) where

import           Control.TimeWarp.Rpc       (NetworkAddress)
import           Options.Applicative.Simple (Parser, auto, help, long, many, metavar,
                                             option, showDefault, simpleOptions,
                                             strOption, switch, value)
import           Serokell.Util.OptParse     (fromParsec)
import           Universum

import           Pos.CLI                    (addrParser, dhtKeyParser, dhtNodeParser,
                                             sscAlgoParser)
import           Pos.DHT.Model              (DHTKey, DHTNode)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))


data Args = Args
    { dbPath             :: !FilePath
    , rebuildDB          :: !Bool
    , spendingGenesisI   :: !(Maybe Int)
    , vssGenesisI        :: !(Maybe Int)
    , spendingSecretPath :: !(Maybe FilePath)
    , vssSecretPath      :: !(Maybe FilePath)
    , port               :: !Word16
    , flatDistr          :: !(Maybe (Int, Int))
    , bitcoinDistr       :: !(Maybe (Int, Int))
    , dhtPeers           :: ![DHTNode]
    , supporterNode      :: !Bool
    , dhtKey             :: !(Maybe DHTKey)
    , logConfig          :: !(Maybe FilePath)
    , logsPrefix         :: !(Maybe FilePath)
    , timeLord           :: !Bool
    , dhtExplicitInitial :: !Bool
    , enableStats        :: !Bool
    , jlPath             :: !(Maybe FilePath)
    , sscAlgo            :: !SscAlgo
    , memoryMode         :: !Bool
    , maliciousEmulation :: ![NetworkAddress]
#ifdef WITH_WEB
    , enableWeb          :: !Bool
    , webPort            :: !Word16
#endif
    , disablePropagation :: !Bool
    }
  deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    strOption
        (long "db-path" <> metavar "FILEPATH" <> value "node-db" <>
        help "Path to the node database") <*>
    switch
        (long "rebuild-db" <>
         help
             "If we DB already exist, discard it's contents and create new one from scratch") <*>
    optional
        (option
             auto
             (long "spending-genesis" <> metavar "INT" <>
              help "Use genesis spending #i")) <*>
    optional
        (option
             auto
             (long "vss-genesis" <> metavar "INT" <> help "Use genesis vss #i")) <*>
    optional
        (strOption
             (long "spending-sk" <> metavar "FILEPATH" <>
              help "Path to spending secret key")) <*>
    optional
        (strOption
             (long "vss-sk" <> metavar "FILEPATH" <>
              help "Path to VSS secret key")) <*>
    option
        auto
        (long "port" <> metavar "INTEGER" <> value 3000 <> showDefault <>
         help "Port to work on") <*>
    optional
        (option auto $
         mconcat
             [ long "flat-distr"
             , metavar "(INT,INT)"
             , help
                   "Use flat stake distribution with given parameters (nodes, coins)"
             ]) <*>
    optional
        (option auto $
         mconcat
             [ long "bitcoin-distr"
             , metavar "(INT,INT)"
             , help
                   "Use bitcoin stake distribution with given parameters (nodes, coins)"
             ]) <*>
    many
        (option (fromParsec dhtNodeParser) $
         long "peer" <> metavar "HOST:PORT/HOST_ID" <> help peerHelpMsg) <*>
    switch
        (long "supporter" <> help "Launch DHT supporter instead of full node") <*>
    optional
        (option (fromParsec dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <> help "DHT key in base64-url") <*>
    optional
        (strOption $
         long "log-config" <> metavar "FILEPATH" <> help "Path to logger configuration")
    <*>
    optional
        (strOption $
         long "logs-prefix" <> metavar "FILEPATH" <> help "Prefix to logger output path")
    <*>
    switch
        (long "time-lord" <>
         help
             "Peer is time lord, i.e. one responsible for system start time decision & propagation (used only in development)") <*>
    switch
        (long "explicit-initial" <>
         help
             "Explicitely contact to initial peers as to neighbors (even if they appeared offline once)") <*>
    switch (long "stats" <> help "Enable stats logging") <*>
    optional
        (strOption
        (long "json-log" <> metavar "FILEPATH" <>
         help "Path to json log file")) <*>
    option (fromParsec sscAlgoParser)
        (long "ssc-algo" <> metavar "ALGO" <> value GodTossingAlgo <> showDefault <>
         help "Shared Seed Calculation algorithm which nodes will use") <*>
    switch
        (long "memory-mode" <>
         help "Run DB in memory mode") <*>
    many
        (option (fromParsec addrParser) $
         long "malicious" <> metavar "HOST_ID" <> help "Node to cheat on")
#ifdef WITH_WEB
    <*>
    switch
        (long "web" <>
         help "Run web server") <*>
    option auto
        (long "web-port" <> metavar "PORT" <> value 8080 <> showDefault <>
         help "Port for web server")
#endif
    <*> switch
        (long "disable-propagation" <>
         help "Disable network propagation (transactions, SSC data, blocks). I.e. all data is to be sent only by entity who creates data and entity is yosend it to all peers on his own")
  where
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

getNodeOptions :: IO Args
getNodeOptions = do
    (res, ()) <-
        simpleOptions "cardano-node" "PoS prototype node" "Use it!" argsParser empty
    return res
