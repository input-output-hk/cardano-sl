{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Default               (def)

import           Control.Applicative        (empty)
import           Control.Monad              (fail)
import           Control.TimeWarp.Logging   (Severity (Debug, Info))
import           Data.Binary                (Binary, decode, encode)
import qualified Data.ByteString.Lazy       as LBS
import           Data.List                  ((!!))
import           Data.Monoid                ((<>))
import           Options.Applicative.Simple (Parser, auto, help, long, many, metavar,
                                             option, showDefault, simpleOptions,
                                             strOption, switch, value)
import           Pos.CLI                    (dhtKeyParser, dhtNodeParser)
import           Pos.Constants              (RunningMode (..), runningMode)
import           Pos.Crypto                 (keyGen, vssKeyGen)
import           Pos.DHT                    (DHTKey, DHTNode, DHTNodeType (..),
                                             dhtNodeType)
import           Pos.Genesis                (genesisSecretKeys, genesisUtxoPetty,
                                             genesisVssKeyPairs)
import           Pos.Launcher               (BaseParams (..), LoggingParams (..),
                                             NodeParams (..), runNodeReal,
                                             runSupporterReal, runTimeLordReal,
                                             runTimeSlaveReal)
import           Serokell.Util.OptParse     (fromParsec)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))
import           Universum                  hiding ((<>))

data Args = Args
    { dbPath             :: !FilePath
    , rebuildDB          :: !Bool
    , spendingGenesisI   :: !(Maybe Int)
    , vssGenesisI        :: !(Maybe Int)
    , spendingSecretPath :: !(Maybe FilePath)
    , vssSecretPath      :: !(Maybe FilePath)
    , port               :: !Word16
    , pettyUtxo          :: !Bool
    , dhtPeers           :: ![DHTNode]
    , supporterNode      :: !Bool
    , dhtKey             :: !(Maybe DHTKey)
    , mainLogSeverity    :: !Severity
    , dhtLogSeverity     :: !Severity
    , commLogSeverity    :: !(Maybe Severity)
    , serverLogSeverity  :: !(Maybe Severity)
    , timeLord           :: !Bool
    , dhtExplicitInitial :: !Bool
    }
  deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    strOption
        (long "db-path" <> metavar "FILEPATH" <> value "node-db" <>
         help "Path to the node database") <*>
    switch (long "rebuild-db" <> help "If we DB already exist, discard it's contents and create new one from scratch") <*>
    optional (option auto
        (long "spending-genesis" <> metavar "INT" <>
         help "Use genesis spending #i")) <*>
    optional (option auto
        (long "vss-genesis" <> metavar "INT" <>
         help "Use genesis vss #i")) <*>
    optional (strOption
        (long "spending-sk" <> metavar "FILEPATH" <>
         help "Path to spending secret key")) <*>
    optional(strOption
        (long "vss-sk" <> metavar "FILEPATH" <>
         help "Path to VSS secret key")) <*>
    option
        auto
        (long "port" <> metavar "INTEGER" <> value 3000 <> showDefault <>
         help "Port to work on") <*>
    switch (long "petty-utxo" <> help "Petty utxo (1 coin per transaction, many txs)") <*>
    many
        (option (fromParsec dhtNodeParser) $
         long "peer" <> metavar "HOST:PORT/HOST_ID" <>
         help peerHelpMsg) <*>
    switch (long "supporter" <> help "Launch DHT supporter instead of full node") <*>
    optional
        (option (fromParsec dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <>
         help "DHT key in base64-url") <*>
    option auto
        (long "main-log" <> metavar "SEVERITY" <> value Debug <> showDefault <>
         help "Main log severity, one of Info, Debug, Warning, Error") <*>
    option auto
        (long "dht-log" <> metavar "SEVERITY" <> value Info <> showDefault <>
         help "DHT log severity, one of Info, Debug, Warning, Error") <*>
    optional
        (option auto $ mconcat
        [long "comm-log",
         metavar "SEVERITY",
         help "Comm (time-warp) log severity"
        ]) <*>
    optional
        (option auto $ mconcat
        [long "server-log",
         metavar "SEVERITY",
         help "Server log severity"
        ]) <*>
    switch (long "time-lord" <> help "Peer is time lord, i.e. one responsible for system start time decision & propagation (used only in development)") <*>
    switch (long "explicit-initial" <> help "Explicitely contact to initial peers as to neighbors (even if they appeared offline once)")
  where
    peerHelpMsg = "Peer to connect to for initial peer discovery. Format example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

getKey :: Binary key => Maybe key -> Maybe FilePath -> FilePath -> IO key -> IO key
getKey (Just key) _ _ _ = return key
getKey _ (Just path) _ _ = decode' path
getKey _ _ fpath gen = do
    createDirectoryIfMissing True "pos-keys"
    decode' ("pos-keys" </> fpath) `catch` \(_ :: SomeException) -> do
        key <- gen
        LBS.writeFile ("pos-keys" </> fpath) $ encode key
        putStrLn $ "Generated key " ++ ("pos-keys" </> fpath)
        return key

decode' :: Binary key => FilePath -> IO key
decode' fpath = either fail' return . decode =<< LBS.readFile fpath
  where
    fail' e = fail $ "Error reading key from " ++ fpath ++ ": " ++ e

main :: IO ()
main = do
    (args@Args {..},()) <-
        simpleOptions "pos-node" "PoS prototype node" "Use it!" argsParser empty
    case dhtKey of
      Just key -> do
        let type_ = dhtNodeType key
        if type_ == Just (if supporterNode then DHTSupporter else DHTFull)
          then return ()
          else case type_ of
                 Just type_' -> fail $ "Id of type " ++ (show type_') ++ " supplied"
                 _           -> fail "Id of unknown type supplied"
      _ -> return ()
    if supporterNode
       then runSupporterReal (baseParams "supporter" args)
       else do
          spendingSK <- getKey ((genesisSecretKeys !!) <$> spendingGenesisI) spendingSecretPath "spending" (snd <$> keyGen)
          vssSK <- getKey ((genesisVssKeyPairs !!) <$> vssGenesisI) vssSecretPath "vss.keypair" vssKeyGen
          systemStart <- getSystemStart args
          runNodeReal $ params args spendingSK vssSK systemStart
  where
    getSystemStart args =
      case runningMode of
        Development -> if timeLord args
                          then runTimeLordReal (loggingParams "time-lord" args)
                          else runTimeSlaveReal (baseParams "time-slave" args)
        Production systemStart -> return systemStart
    loggingParams logger Args{..} =
        def
        { lpRootLogger = logger
        , lpMainSeverity = mainLogSeverity
        , lpDhtSeverity = Just dhtLogSeverity
        , lpServerSeverity = serverLogSeverity
        , lpCommSeverity = commLogSeverity
        }
    baseParams logger args@Args{..} =
        BaseParams
        { bpLogging = loggingParams logger args
        , bpPort = port
        , bpDHTPeers = dhtPeers
        , bpDHTKeyOrType = if supporterNode
                              then maybe (Right DHTSupporter) Left dhtKey
                              else maybe (Right DHTFull) Left dhtKey
        , bpDHTExplicitInitial = dhtExplicitInitial
        }
    params args@Args{..} spendingSK vssSK systemStart =
        NodeParams
        { npDbPath = Just dbPath
        , npRebuildDb = rebuildDB
        , npSystemStart = systemStart
        , npSecretKey = spendingSK
        , npVssKeyPair = vssSK
        , npBaseParams = baseParams "node" args
        , npCustomUtxo = if pettyUtxo
                         then Just $ genesisUtxoPetty 20000
                         else Nothing
        }
