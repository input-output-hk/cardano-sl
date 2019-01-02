module Main where

-- fixme: this overlaps cardano-shell and the demo cluster script

import Turtle hiding (switch, f)
import Prelude hiding (FilePath)
import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as FP
import Data.Maybe (fromMaybe)

-- | Parameters for connecting to a Cardano SL blockchain network
data NetworkSettings = NetworkSettings
  { netRelays :: Text -- ^ DNS name for public relay(s)
  , netConfigKey :: Text -- ^ Top-level section of the YAML config file
  , netConfigFile :: FilePath -- ^ YAML config file
  } deriving (Show)

data Network = Mainnet | Staging | Testnet | Demo
  deriving (Show, Eq)

data NodeType = Wallet | Explorer
  deriving (Show, Eq)

networkSettings :: Network -> NetworkSettings
networkSettings Mainnet = NetworkSettings
  { netRelays = "relays.cardano-mainnet.iohk.io"
  , netConfigKey = "mainnet_full"
  , netConfigFile = defaultConfigFile
  }
networkSettings Staging = NetworkSettings
  { netRelays = "relays.awstest.iohkdev.io"
  , netConfigKey = "mainnet_dryrun_full"
  , netConfigFile = defaultConfigFile
  }
networkSettings Testnet = NetworkSettings
  { netRelays = "relays.cardano-testnet.iohkdev.io"
  , netConfigKey = "testnet_full"
  , netConfigFile = defaultConfigFile
  }
networkSettings Demo = NetworkSettings
  { netRelays = "localhost"
  , netConfigKey = "dev"
  , netConfigFile = defaultConfigFile
  }

defaultConfigFile :: FilePath
defaultConfigFile = "lib/configuration.yaml"

defaultRTSOptions :: String
defaultRTSOptions = "-N2 -qg -A1m -I0 -T"

connectScriptOptionsParser :: Parser (Phase, ConnectParams)
connectScriptOptionsParser = (,) <$> phaseP <*> connectParamsP

data Phase = FullRun | OnlySetup | SkipSetup deriving (Show, Eq)

phaseP :: Parser Phase
phaseP = flag' OnlySetup (long "only-setup" <> help "Just run setup phase")
  <|> flag' SkipSetup (long "skip-setup" <> help "Don't run setup phase")
  <|> pure FullRun

data ConnectParams = ConnectParams
  { paramsNodeType :: NodeType
  , paramsNetwork :: Either NetworkSettings Network
  , paramsRTSOptions :: Text
  , paramsStateDir :: Maybe FilePath
  , paramsDeleteStateDir :: Bool
  , paramsExtraArgs :: [Text]
  } deriving Show

connectParamsP :: Parser ConnectParams
connectParamsP = ConnectParams
                 <$> nodeTypeP
                 <*> networkP
                 <*> rtsOptionsP
                 <*> optional stateDirP
                 <*> deleteStateP
                 <*> paramsExtraArgsP
  where
    nodeTypeP = argument readNodeType (metavar "NODE"
                                       <> help "Type of node to connect (wallet or explorer)")
    networkP = (Right <$> predefNetworkP) <|> (Left <$> networkSettingsP)
    predefNetworkP = argument readNetwork (metavar "NETWORK"
                                           <> help "Network to connect to (mainnet, staging, or testnet)")
    networkSettingsP = NetworkSettings
      <$> strOption (long "relays" <> metavar "HOSTNAME" <> help "DNS name for public relay(s)")
      <*> strOption (long "configuration-key" <> metavar "NAME" <> help "Top-level section of the YAML config file")
      <*> strOption (long "configuration-file" <> metavar "PATH" <> help "YAML config file")

    rtsOptionsP = T.pack <$> strOption ( long "rts" <> metavar "RTSOPTS"
                                         <> showDefault <> value defaultRTSOptions
                                         <> help "GHC RTS options to pass to subprocess" )
    stateDirP = FP.fromText . T.pack <$> strOption ( long "state-dir" <> metavar "DIR"
                                                  <> help "State directory (default: ./state-TYPE-NETWORK)" )
    deleteStateP = switch ( long "delete-state" <> help "Remove state directory before starting" )

    paramsExtraArgsP = many (strArgument (metavar "ARGS..."
                                           <> help "Pass through arguments (put after -- )"))

readNodeType :: ReadM NodeType
readNodeType = maybeReader (p . T.toLower . T.pack)
  where
    p "wallet" = Just Wallet
    p "explorer" = Just Explorer
    p _ = Nothing

readNetwork :: ReadM Network
readNetwork = maybeReader (p . T.toLower . T.pack)
  where
    p "mainnet" = Just Mainnet
    p "staging" = Just Staging
    p "testnet" = Just Testnet
    p _ = Nothing

main :: IO ()
main = uncurry connectScript =<< execParser opts
  where
    opts = info (connectScriptOptionsParser <**> helper)
      ( fullDesc
        <> progDesc "Run a Cardano SL node to connect to a network." )

doSetup :: Phase -> Bool
doSetup FullRun = True
doSetup OnlySetup = True
doSetup SkipSetup = False

doNode :: Phase -> Bool
doNode FullRun = True
doNode OnlySetup = False
doNode SkipSetup = True

connectScript :: Phase -> ConnectParams -> IO ()
connectScript phase c = do
  let st = stateDir c
      ns = either id networkSettings (paramsNetwork c)

  when (doSetup phase) $ do
    setupStateDir c st
    when (paramsNodeType c == Wallet) $ generateCerts st ns

  when (doNode phase) $ do
    printf ("Launching a node connected to "%w%" ...\n") (paramsNetwork c)
    topology <- makeTopologyFile st ns
    launchNode c st topology ns

setupStateDir :: ConnectParams -> FilePath -> IO ()
setupStateDir c st = do
  when (paramsDeleteStateDir c) $ do
    printf ("Deleting "%fp%" ...\n") st
    exists <- testpath st
    when exists $ rmtree st

  printf ("Keeping state in "%fp%"\n") st
  mktree (st </> "logs")

run :: Text -> [Text] -> IO ()
run exe args = do
  T.putStrLn (T.unwords (exe:args))
  procs exe args empty

launchNode :: ConnectParams -> FilePath -> FilePath -> NetworkSettings -> IO ()
launchNode c st topology ns = run exe args
  where
    exe = programName (paramsNodeType c)
    args = configArgs ns ++ walletArgs ++
           [ "--log-config", tt ("log-configs" </> "connect-to-cluster.yaml")
           , "--topology", tt topology
           , "--logs-prefix", tt (st </> "logs")
           , "--db-path", tt (st </> "db")
           , "--keyfile", tt (st </> "secret.key")
           ] ++ rtsArgs ++ paramsExtraArgs c
    walletArgs = if paramsNodeType c == Wallet then walletArgs' else []
    walletArgs' = [ "--tlscert", tt (st </> "tls" </> "server" </> "server.crt")
                  , "--tlskey", tt (st </> "tls" </> "server" </> "server.key")
                  , "--tlsca", tt (st </> "tls" </> "server" </> "ca.crt")
                  , "--wallet-db-path", tt (st </> "wallet-db")
                  ]
    rtsArgs | T.null (paramsRTSOptions c) = []
            | otherwise = [ "+RTS", paramsRTSOptions c, "-RTS" ]

generateCerts :: FilePath -> NetworkSettings -> IO ()
generateCerts st ns = do
  mktree server
  mktree client
  run "cardano-x509-certificates" args
  where
    server = st </> "tls" </> "server"
    client = st </> "tls" </> "client"
    args = [ "--server-out-dir", tt server
           , "--clients-out-dir", tt client
           ] ++ configArgs ns

configArgs :: NetworkSettings -> [Text]
configArgs ns = [ "--configuration-file", tt (netConfigFile ns)
                , "--configuration-key", netConfigKey ns ]

tlShow :: Show a => a -> Text
tlShow = T.toLower . T.pack . show

sl :: Show a => Format r (a -> r)
sl = makeFormat tlShow

tt :: FilePath -> Text
tt = format fp

stateDir :: ConnectParams -> FilePath
stateDir c = fromMaybe (FP.fromText defaultStateDir) (paramsStateDir c)
  where
    defaultStateDir = format ("./state-"%sl%"-"%s) (paramsNodeType c) netName
    netName = either (const "custom") tlShow (paramsNetwork c)

makeTopologyFile :: FilePath -> NetworkSettings -> IO FilePath
makeTopologyFile st n = writeTopologyFile topologyFile n >> pure topologyFile
  where topologyFile = st </> "topology.yaml"

writeTopologyFile :: FilePath -> NetworkSettings -> IO ()
writeTopologyFile f n = T.writeFile (FP.encodeString f) topology
  where
    topology = T.unlines
      [ "wallet:"
      , "  relays: [[{ host: " <> netRelays n <> " }]]"
      , "  valency: 1"
      , "  fallbacks: 7"
      ]

-- | Returns the executable name for a given node type
programName :: NodeType -> Text
programName Wallet = "cardano-node"
programName Explorer = "cardano-explorer"
