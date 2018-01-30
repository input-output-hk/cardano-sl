{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with command-line options.

module Bench.Pos.Wallet.Config.CLI
    ( getOptions
    ) where

import           Universum

import           Data.Monoid            ((<>))
import           Options.Applicative    (Parser,
                                         execParser, fullDesc, header,
                                         help, helper, info, long,
                                         metavar, progDesc,
                                         strOption, switch)

import           Bench.Pos.Wallet.Types (CLOptions (..))

-- | Parser for command-line options.
optionsParser :: Parser CLOptions
optionsParser = do
    pathToEndpointsConfig
        <- strOption $
           long        "ep-conf"
        <> metavar     "PATH_TO_ENDPOINTS_CONFIG"
        <> help        "Path to endpoints configuration .csv-file"
    pathToWalletsConfig
        <- strOption $
           long        "wal-conf"
        <> metavar     "PATH_TO_WALLETS_CONFIG"
        <> help        "Path to wallets configuration .yaml-file"
    pathToTLSPubCert
        <- strOption $
           long        "tls-pub-cert"
        <> metavar     "PATH_TO_TLS_PUB_CERT"
        <> help        "Path to TLS public certificate"
    pathToTLSPrivKey
        <- strOption $
           long        "tls-priv-key"
        <> metavar     "PATH_TO_TLS_PRIV_KEY"
        <> help        "Path to TLS private key"
    runConcurrently
        <- switch $
           long        "async"
        <> help        "If defined - run benchmarks concurrently"
    return CLOptions{..}

-- | Get command-line options.
getOptions :: IO CLOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool for Wallet Web API benchmarking."
