{-# LANGUAGE CPP #-}

-- | Command line options of cardano-wallet

module WalletOptions
       ( WalletOptions (..)
       , WalletAction (..)
       , optsInfo
       ) where

import           Options.Applicative    (CommandFields, Mod, Parser, ParserInfo, auto,
                                         command, fullDesc, help, helper, info, long,
                                         many, metavar, option, option, progDesc, short,
                                         showDefault, subparser, switch, value)
import           Serokell.Util.OptParse (fromParsec, strOption)
import           Universum


import           Pos.CLI                (dhtNodeParser, sscAlgoParser)
import           Pos.DHT.Model          (DHTNode)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))

data WalletOptions = WalletOptions
    { woDbPath             :: !FilePath
    , woRebuildDb          :: !Bool
    , woDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , woDhtExplicitInitial :: !Bool
    , woPort               :: !Word16     -- ^ DHT/Blockchain port
    , woInitialPause       :: !Int        -- ^ Pause between connecting to network
                                          -- and starting accepting commands (in slots)
    , woKeyFilePath        :: !FilePath   -- ^ Path to file with secret keys
    , woDebug              :: !Bool       -- ^ Run in debug mode (with genesis keys included)
    , woLogConfig          :: !(Maybe FilePath)
    , woLogsPrefix         :: !(Maybe FilePath)
    , woJLFile             :: !(Maybe FilePath)
    , woSscAlgo            :: !SscAlgo
    , woFlatDistr          :: !(Maybe (Int, Int))
    , woBitcoinDistr       :: !(Maybe (Int, Int))
    , woDisablePropagation :: !Bool
    , woAction             :: !WalletAction
    }

data WalletAction = Repl
#ifdef WITH_WEB
                  | Serve { webPort           :: !Word16
                          , webDaedalusDbPath :: !FilePath
                          }
#endif

actionParser :: Parser WalletAction
actionParser = subparser $ replParser
#ifdef WITH_WEB
                        <> serveParser
#endif

replParser :: Mod CommandFields WalletAction
replParser = command "repl" $ info (pure Repl) $
             progDesc "Run REPL in console to evaluate the commands"

#ifdef WITH_WEB
serveParser :: Mod CommandFields WalletAction
serveParser = command "serve" $ info opts desc
  where opts = Serve <$> option auto (long "web-port"
                                   <> metavar "PORT"
                                   <> value 8090    -- to differ from node's default port
                                   <> showDefault
                                   <> help "Port for web server")
                     <*> option auto (long "daedalus-db-path"
                                   <> metavar "FILEPATH"
                                   <> value "run/daedalus-db"
                                   <> help "Path to the wallet database")
        desc = progDesc "Serve HTTP Daedalus API on given port"
#endif

optionsParser :: Parser WalletOptions
optionsParser = WalletOptions
    <$> strOption (long "db-path"
                <> metavar "FILEPATH"
                <> value "wallet-db"
                <> help "Path to the wallet database")
    <*> switch (long "rebuild-db"
             <> help ("If we DB already exist, discard it's contents and " <>
                      "create new one from scratch"))
    <*> many (option (fromParsec dhtNodeParser) $
              long "peer"
           <> metavar "HOST:PORT/HOST_ID"
           <> help "Initial DHT peer (may be many)")
    <*> switch (long "explicit-initial"
             <> help ("Explicitely contact to initial peers as to neighbors " <>
                      "(even if they appeared offline once)"))
    <*> option auto (long "port"
                  <> metavar "PORT"
                  <> value 24961   -- truly random value
                  <> showDefault
                  <> help "Port to work on")
    <*> option auto (long "initial-pause"
                  <> short 'p'
                  <> value 1
                  <> metavar "SLOTS_NUM"
                  <> help "Pause between connecting to network and starting accepting commands")
    <*> strOption (long "keys-path"
                <> metavar "FILEPATH"
                <> value "secret.key"
                <> help "Path to file with secret keys")
    <*> switch (long "debug"
             <> help "Run in debug mode (with genesis keys included)")
    <*> optional (strOption $
                  long "log-config"
               <> metavar "FILEPATH"
               <> help "Path to logger configuration")
    <*> optional (strOption $
                  long "logs-prefix"
               <> metavar "FILEPATH"
               <> help "Prefix to logger output path")
    <*> optional (strOption $
                  long "json-log"
               <> metavar "FILEPATH"
               <> help "Path to json log file")
    <*> option (fromParsec sscAlgoParser)
        (long "ssc-algo"
      <> metavar "ALGO"
      <> value GodTossingAlgo
      <> showDefault
      <> help "Shared Seed Calculation algorithm which nodes will use")
    <*> optional
        (option auto $
         mconcat
            [ long "flat-distr"
            , metavar "(INT,INT)"
            , help "Use flat stake distribution with given parameters (nodes, coins)"
            ])
    <*> optional
        (option auto $
         mconcat
            [ long "bitcoin-distr"
            , metavar "(INT,INT)"
            , help "Use bitcoin stake distribution with given parameters (nodes, coins)"
            ])
    <*> switch
        (long "disable-propagation" <>
         help "Disable network propagation (transactions, SSC data, blocks). I.e. all data is to be sent only by entity who creates data and entity is yosend it to all peers on his own")
    <*> actionParser

optsInfo :: ParserInfo WalletOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Wallet-only node"
