{-# LANGUAGE CPP #-}

#ifdef WITH_WALLET
-- | Wallet's command line options.

module WalletOptions
       ( WalletCommand (..)
       , WalletOptions (..)
       , getWalletOptions
       ) where

import           Universum

import qualified Options.Applicative    as Opts
import           Serokell.Util.OptParse (fromParsec)

import           Pos.CLI                (dhtNodeParser)
import           Pos.DHT                (DHTNode)

data WalletCommand
    = SubmitTx { stGenesisIdx :: !Word -- ^ Index in genesis key pairs.
               , stDHTPeers   :: ![DHTNode]
               , stLogConfig  :: !(Maybe FilePath)
               , stLogsPrefix :: !(Maybe FilePath)}
#ifdef WITH_WEB
    | ServeWallet { swPort :: !Word16}
#endif

commandParser :: Opts.Parser WalletCommand
commandParser =
    Opts.subparser
        (mconcat
             [ Opts.command
                   "submit"
                   (Opts.info submitTxOpts (Opts.progDesc "Submit transactions"))
#ifdef WITH_WEB
             , Opts.command
                   "serve"
                   (Opts.info serveWalletOpts (Opts.progDesc "Run web server"))
#endif
             ])
  where
    submitTxOpts =
        SubmitTx <$>
        Opts.option
            Opts.auto
            (mconcat
                 [ Opts.short 'i'
                 , Opts.long "index"
                 , Opts.metavar "INT"
                 , Opts.help "Index in list of genesis key pairs"
                 ]) <*>
        Opts.many
            (Opts.option (fromParsec dhtNodeParser) $
             Opts.long "peer" <> Opts.metavar "HOST:PORT/HOST_ID" <> Opts.help peerHelpMsg) <*>
        optional
            (Opts.option Opts.auto $
             Opts.long "log-config" <> Opts.metavar "FILEPATH" <>
             Opts.help "Path to logger configuration") <*>
        optional
            (Opts.option Opts.auto $
             Opts.long "logs-prefix" <> Opts.metavar "FILEPATH" <>
             Opts.help "Prefix to logger output path")
#ifdef WITH_WEB
    serveWalletOpts =
        ServeWallet <$>
        Opts.option
            Opts.auto
            (mconcat
                 [ Opts.long "port"
                 , Opts.metavar "PORT"
                 , Opts.help "Port for web server"
                 , Opts.value 8090
                 , Opts.showDefault
                 ])
#endif
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

data WalletOptions = WalletOptions
    { woCommand :: !WalletCommand
    }

optionsParser :: Opts.Parser WalletOptions
optionsParser = WalletOptions <$> commandParser

getWalletOptions :: IO WalletOptions
getWalletOptions =
    Opts.execParser $
    Opts.info
        (Opts.helper <*> optionsParser)
        (Opts.fullDesc `mappend` Opts.progDesc "Stupid wallet")
#endif
