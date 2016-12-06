{-# LANGUAGE CPP #-}

-- | Command line options of cardano-wallet

module WalletOptions
       ( WalletOptions (..)
       , optsInfo
       ) where

import           Options.Applicative    (Parser, ParserInfo, auto, fullDesc, help, helper,
                                         info, long, many, metavar, option, option,
                                         progDesc, short, showDefault, switch, value)
import           Serokell.Util.OptParse (fromParsec, strOption)
import           Universum


import           Pos.CLI                (dhtNodeParser, sscAlgoParser)
import           Pos.DHT                (DHTNode)
import           Pos.Ssc.SscAlgo        (SscAlgo (..))


data WalletOptions = WalletOptions
    { woDbPath             :: !FilePath
    , woRebuildDb          :: !Bool
    , woDHTPeers           :: ![DHTNode]  -- ^ Initial DHT nodes
    , woDhtExplicitInitial :: !Bool
    , woPort               :: !Word16     -- ^ DHT/Blockchain port
#ifdef WITH_WEB
    , woWebPort            :: !Word16     -- ^ A port on which web API listens
#endif
    , woInitialPause       :: !Int     -- ^ Pause between connecting to network
                                       -- and starting accepting commands (in slots)
    , woSecretKeyIdx       :: !Int     -- ^ Index of genesis SK to use
    , woLogConfig          :: !(Maybe FilePath)
    , woLogsPrefix         :: !(Maybe FilePath)
    , woJLFile             :: !(Maybe FilePath)
    , woSscAlgo            :: !SscAlgo
    , woFlatDistr          :: !(Maybe (Int, Int))
    , woBitcoinDistr       :: !(Maybe (Int, Int))
    }

optionsParser :: Parser WalletOptions
optionsParser = WalletOptions
    <$> strOption (long "db-path"
                <> metavar "FILEPATH"
                <> value "wallet-db"
                <> help "Path to the wallet database")
    <*> switch (long "rebuild-db"
             <> help "If we DB already exist, discard it's contents and \
                     \create new one from scratch")
    <*> many (option (fromParsec dhtNodeParser) $
              long "peer"
           <> metavar "HOST:PORT/HOST_ID"
           <> help "Initial DHT peer (may be many)")
    <*> switch (long "explicit-initial"
             <> help "Explicitely contact to initial peers as to neighbors\
                     \ (even if they appeared offline once)")
    <*> option auto (long "port"
                  <> metavar "PORT"
                  <> value 24961   -- truly random value
                  <> showDefault
                  <> help "Port to work on")
#ifdef WITH_WEB
    <*> option auto (long "web-port"
                  <> metavar "PORT"
                  <> value 8090    -- to differ from node's default port
                  <> showDefault
                  <> help "Port for web server")
#endif
    <*> option auto (long "initial-pause"
                  <> short 'p'
                  <> value 1
                  <> metavar "SLOTS_NUM"
                  <> help "Pause between connecting to network and starting\
                          \ accepting commands")
    <*> option auto (long "genesis-sk"
                  <> short 'i'
                  <> metavar "INDEX"
                  <> help "Index of genesis SK to use")
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

optsInfo :: ParserInfo WalletOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Wallet-only node"

