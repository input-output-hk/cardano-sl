{-# LANGUAGE CPP #-}

-- | Command line options of cardano-wallet

module WalletOptions
       ( WalletOptions (..)
       , WalletAction (..)
       , optsInfo
       ) where

import           Options.Applicative    (CommandFields, Mod, Parser, ParserInfo, auto,
                                         command, fullDesc, help, helper, info, long,
                                         metavar, option, option, progDesc, short,
                                         subparser, switch, value)
import           Serokell.Util.OptParse (strOption)
import           Universum


import qualified Pos.CLI                as CLI

data WalletOptions = WalletOptions
    { woDbPath             :: !FilePath
    , woRebuildDb          :: !Bool
    , woPort               :: !Word16     -- ^ DHT/Blockchain port
    , woInitialPause       :: !Int     -- ^ Pause between connecting to network
                                       -- and starting accepting commands (in slots)
    , woJLFile             :: !(Maybe FilePath)
    , woCommonArgs         :: !CLI.CommonArgs -- ^ Common CLI args, including initial DHT nodes
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
  where opts = Serve <$> CLI.webPortOption 8090    -- to differ from node's default port
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
    <*> CLI.portOption 24961    -- truly random value
    <*> option auto (long "initial-pause"
                  <> short 'p'
                  <> value 1
                  <> metavar "SLOTS_NUM"
                  <> help "Pause between connecting to network and starting accepting commands")
    <*> CLI.optionalJSONPath
    <*> CLI.commonArgsParser "Initial DHT peer (may be many)"
    <*> actionParser

optsInfo :: ParserInfo WalletOptions
optsInfo = info (helper <*> optionsParser) $
    fullDesc `mappend` progDesc "Wallet-only node"
