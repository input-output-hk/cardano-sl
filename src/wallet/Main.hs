{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug))
import           Control.TimeWarp.Rpc     (localhost)
import           Data.Default             (def)
import           Data.List                ((!!))
import qualified Options.Applicative      as Opts
import           Universum

import           Pos.Crypto               (unsafeHash)
import           Pos.DHT                  (DHTNodeType (..))
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (LoggingParams (..), NodeParams (..),
                                           submitTxReal)

data WalletCommand = SubmitTx
    { stGenesisIdx :: !Word   -- ^ Index in genesis key pairs.
    , stPort       :: !Word16 -- ^ Port where pos-node is running.
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
        Opts.option
            Opts.auto
            (mconcat
                 [ Opts.short 'p'
                 , Opts.long "port"
                 , Opts.metavar "PORT"
                 , Opts.help "Port where pos-node is running"
                 ])

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
                    , npSystemStart = Nothing
                    , npLogging = def { lpMainSeverity = Debug }
                    , npSecretKey = genesisSecretKeys !! i
                    , npVssKeyPair = genesisVssKeyPairs !! i
                    , npPort = 24962
                    , npDHTPeers = []
                    , npDHTKeyOrType = Right DHTClient
                    }
            let na = (localhost, stPort)
            let addr = genesisAddresses !! i
            let txId = unsafeHash addr
            submitTxReal params [na] (txId, 0) (addr, 10)
