{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug))
import           Data.List                ((!!))
import qualified Options.Applicative      as Opts
import           Universum

import           Pos.Crypto               (unsafeHash)
import           Pos.Genesis              (genesisAddresses, genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (NodeParams (..), submitTxReal)

data WalletCommand = SubmitTx
    { stGenesisIdx :: !Word -- ^ Index in genesis key pairs.
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
        SubmitTx i -> do
            let params =
                    NodeParams
                    { npDbPath = Nothing
                    , npRebuildDb = False
                    , npSystemStart = Nothing
                    , npLoggerName = "wallet"
                    , npLoggingSeverity = Debug
                    , npSecretKey = genesisSecretKeys !! fromIntegral i
                    , npVssKeyPair = genesisVssKeyPairs !! fromIntegral i
                    , npPort = 1000
                    , npDHTPort = 2000
                    , npDHTPeers = []
                    }
            let addr = genesisAddresses !! fromIntegral i
            let txId = unsafeHash addr
            submitTxReal params (txId, 0) (addr, 10)
