{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module NodeOptions
       ( Args (..)
       , getNodeOptions
       ) where

#ifdef WITH_WALLET
import           Options.Applicative.Simple (showDefault)
#endif
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             simpleOptions, strOption, switch, value)
import           Serokell.Util.OptParse     (fromParsec)
import           Universum

import qualified Pos.CLI                    as CLI
import           Pos.DHT.Model              (DHTKey)
import           Pos.Security.Types         (AttackTarget, AttackType)


data Args = Args
    { dbPath                    :: !FilePath
    , rebuildDB                 :: !Bool
    , spendingGenesisI          :: !(Maybe Int)
    , vssGenesisI               :: !(Maybe Int)
    , keyfilePath               :: !FilePath
    , vssSecretPath             :: !(Maybe FilePath)
    , port                      :: !Word16
    , supporterNode             :: !Bool
    , dhtKey                    :: !(Maybe DHTKey)
    , timeLord                  :: !Bool
    , enableStats               :: !Bool
    , jlPath                    :: !(Maybe FilePath)
    , maliciousEmulationAttacks :: ![AttackType]
    , maliciousEmulationTargets :: ![AttackTarget]
#ifdef WITH_WEB
    , enableWeb                 :: !Bool
    , webPort                   :: !Word16
#ifdef WITH_WALLET
    , enableWallet              :: !Bool
    , walletPort                :: !Word16
    , walletDbPath              :: !FilePath
    , walletDebug               :: !Bool
#endif
#endif
    , commonArgs                :: !CLI.CommonArgs
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
             "If we DB already exist, discard it's contents and create new one from\
             \ scratch") <*>
    optional
        (option
             auto
             (long "spending-genesis" <> metavar "INT" <>
              help "Use genesis spending #i")) <*>
    optional
        (option
             auto
             (long "vss-genesis" <> metavar "INT" <> help "Use genesis vss #i")) <*>
    strOption
        (long "keyfile" <>
         metavar "FILEPATH" <>
         value "secret.key" <>
         help "Path to file with secret keys") <*>
    optional
        (strOption
             (long "vss-sk" <> metavar "FILEPATH" <>
              help "Path to VSS secret key")) <*>
    CLI.portOption 3000 <*>
    switch
        (long "supporter" <> help "Launch DHT supporter instead of full node") <*>
    optional
        (option (fromParsec CLI.dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <> help "DHT key in base64-url") <*>
    CLI.timeLordOption <*>
    switch (long "stats" <> help "Enable stats logging") <*>
    CLI.optionalJSONPath <*>
    many
        (option (fromParsec CLI.attackTypeParser) $
         long "attack" <> metavar "NoBlocks|NoCommitments"
         <> help "Attack type to emulate") <*>
    many
        (option (fromParsec CLI.attackTargetParser) $
         long "attack-target" <> metavar "HOST:PORT|PUBKEYHASH")
#ifdef WITH_WEB
    <*>
    switch
        (long "web" <>
         help "Run web server") <*>
    CLI.webPortOption 8080 "Port for web server"
#ifdef WITH_WALLET
    <*>
    switch
        (long "wallet" <>
         help "Run wallet web api") <*>
    CLI.webPortOption 8090 "Port for Daedalus Wallet API" <*>
    strOption
        (long "wallet-db-path" <>
         help "Path to the wallet acid-state" <>
         value "wallet-db") <*>
    switch
        (long "wallet-debug" <>
         help "Run wallet with debug params (e. g. include all the genesis keys in the set of secret keys)")
#endif
#endif
    <*> CLI.commonArgsParser peerHelpMsg
  where
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format\
        \ example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

getNodeOptions :: IO Args
getNodeOptions = do
    (res, ()) <-
        simpleOptions "cardano-node" "PoS prototype node" "Use it!" argsParser empty
    return res
