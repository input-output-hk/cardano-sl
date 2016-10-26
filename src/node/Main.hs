{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Default               (def)

import           Control.Applicative        (empty)
import           Control.TimeWarp.Logging   (Severity (Debug))
import           Data.List                  ((!!))
import           Data.Monoid                ((<>))
import           Options.Applicative.Simple (Parser, auto, help, long, many, metavar,
                                             option, showDefault, simpleOptions,
                                             strOption, switch, value)
import           Pos.Slotting               (Timestamp (..))
import           Universum                  hiding ((<>))

import           Control.Monad              (fail)
import           Data.Binary                (Binary, decode, encode)
import qualified Data.ByteString.Lazy       as LBS
import           Pos.CLI                    (dhtKeyParser, dhtNodeParser)
import           Pos.Crypto                 (keyGen, vssKeyGen)
import           Pos.DHT                    (DHTKey, DHTNode, DHTNodeType (..),
                                             dhtNodeType)
import           Pos.Genesis                (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher               (LoggingParams (..), NodeParams (..),
                                             SupporterParams (..), runNodeReal,
                                             runSupporterReal)
import           Serokell.Util.OptParse     (fromParsec)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))

data Args = Args
    { dbPath             :: FilePath
    , rebuildDB          :: Bool
    , spendingGenesisI   :: Maybe Int
    , vssGenesisI        :: Maybe Int
    , spendingSecretPath :: Maybe FilePath
    , vssSecretPath      :: Maybe FilePath
    , port               :: Word16
    , dhtPeers           :: [DHTNode]
    , systemStart        :: !(Maybe Timestamp)
    , supporterNode      :: !Bool
    , dhtKey             :: !(Maybe DHTKey)
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
    many
        (option (fromParsec dhtNodeParser) $
         long "peer" <> metavar "HOST:PORT/HOST_ID" <>
         help peerHelpMsg) <*>
    optional (
      option auto
        (long "start-time" <> metavar "TMESTAMP" <>
         help "Start time")) <*>
    switch (long "supporter" <> help "Launch DHT supporter instead of full node") <*>
    optional
        (option (fromParsec dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <>
         help "DHT key in base64-url")
  where
    peerHelpMsg = "Peer to connect to for initial peer discovery. Format example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

getKey :: Binary key => (Maybe key) -> Maybe FilePath -> FilePath -> IO key -> IO key
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
    (args@(Args {..}),()) <- simpleOptions "pos-node" "PoS prototype node" "Use it!" argsParser empty
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
       then runSupporterReal (supporterParams args)
       else do
          spendingSK <- getKey ((genesisSecretKeys !!) <$> spendingGenesisI) spendingSecretPath "spending" (snd <$> keyGen)
          vssSK <- getKey ((genesisVssKeyPairs !!) <$> vssGenesisI) vssSecretPath "vss.keypair" vssKeyGen
          runNodeReal $ params args spendingSK vssSK
  where
    loggingParams logger =
        def
        { lpRootLogger = logger
        , lpMainSeverity = Debug
        }
    supporterParams (Args {..}) =
        SupporterParams
        { spLogging = loggingParams "supporter"
        , spPort = port
        , spDHTPeers = dhtPeers
        , spDHTKeyOrType = maybe (Right DHTSupporter) Left dhtKey
        }
    params (Args {..}) spendingSK vssSK =
        NodeParams
        { npDbPath = Just dbPath
        , npRebuildDb = rebuildDB
        , npSystemStart = systemStart
        , npLogging = loggingParams "node"
        , npSecretKey = spendingSK
        , npVssKeyPair = vssSK
        , npPort = port
        , npDHTPeers = dhtPeers
        , npDHTKeyOrType = maybe (Right DHTFull) Left dhtKey
        }
