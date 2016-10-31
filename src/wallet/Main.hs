{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug))
import           Data.Default             (def)
import           Data.List                ((!!))
import qualified Options.Applicative      as Opts
import           Universum                hiding ((<>))

import           Data.Monoid              ((<>))

import           Pos.CLI                  (dhtNodeParser)
import           Pos.Crypto               (unsafeHash)
import           Pos.DHT                  (DHTNode, DHTNodeType (..))
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (BaseParams (..), LoggingParams (..),
                                           NodeParams (..), submitTxReal)
import           Serokell.Util.OptParse   (fromParsec)

data WalletCommand = SubmitTx
    { stGenesisIdx :: !Word   -- ^ Index in genesis key pairs.
    , stDHTPeers   :: ![DHTNode]
    }

commandParser :: Opts.Parser WalletCommand
commandParser =
    Opts.subparser
        (Opts.command
             "submit"
             (Opts.info submitTxOpts (Opts.progDesc "Submit transactions")))
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
             Opts.long "peer" <> Opts.metavar "HOST:PORT/HOST_ID" <>
             Opts.help peerHelpMsg)
    peerHelpMsg = "Peer to connect to for initial peer discovery. Format example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

data WalletOptions = WalletOptions
    { woCommand :: !WalletCommand
    }

optionsParser :: Opts.Parser WalletOptions
optionsParser = WalletOptions <$> commandParser

main :: IO ()
main = do
    WalletOptions {..} <-
        Opts.execParser $
        Opts.info
            (Opts.helper <*> optionsParser)
            (Opts.fullDesc `mappend` Opts.progDesc "Stupid wallet")
    case woCommand of
        SubmitTx {..} -> do
            let i = fromIntegral stGenesisIdx
            let params =
                    NodeParams
                    { npDbPath = Nothing
                    , npRebuildDb = False
                    , npSystemStart = 1477706355381569 --arbitrary value
                    , npSecretKey = genesisSecretKeys !! i
                    , npVssKeyPair = genesisVssKeyPairs !! i
                    , npBaseParams = BaseParams
                                      { bpLogging = def { lpMainSeverity = Debug, lpRootLogger = "wallet" }
                                      , bpPort = 24962
                                      , bpDHTPeers = stDHTPeers
                                      , bpDHTKeyOrType = Right DHTClient
                                      }
                    , npCustomUtxo = Nothing
                    }
            let addr = genesisAddresses !! i
            let txId = unsafeHash addr
            submitTxReal params (txId, 0) (addr, 10)
