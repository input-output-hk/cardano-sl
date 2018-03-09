{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with command-line options.

module Bench.Cardano.Wallet.Config.CLI
    ( getOptions
    ) where

import           Universum

import           Options.Applicative        (Parser,
                                             execParser, fullDesc, header,
                                             help, helper, info, long,
                                             metavar, progDesc,
                                             strOption, switch)

import           Bench.Cardano.Wallet.Types (CLOptions (..))

-- | Parser for command-line options.
optionsParser :: Parser CLOptions
optionsParser = do
    pathToEndpointsConfig
        <- strOption $
           long        "ep-conf"
        <> metavar     "ENDPOINTS_CONFIG"
        <> help        "Path to endpoints configuration .csv-file"
    pathToWalletsConfig
        <- strOption $
           long        "wal-conf"
        <> metavar     "WALLETS_CONFIG"
        <> help        "Path to wallets configuration .yaml-file"
    pathToTLSPubCert
        <- strOption $
           long        "tls-pub-cert"
        <> metavar     "PUB_CERT"
        <> help        "Path to TLS public certificate"
    pathToTLSPrivKey
        <- strOption $
           long        "tls-priv-key"
        <> metavar     "PRIV_KEY"
        <> help        "Path to TLS private key"
    runConcurrently
        <- switch $
           long        "async"
        <> help        "If defined - run benchmarks concurrently"
    analyzeResponse
        <- switch $
           long        "analyze"
        <> help        ("If defined - analyze responses from the wallet. " <>
                        "Please note that analysis makes measurement less accurate, " <>
                        "because reports are storing in the file, and this IO-action " <>
                        "will be measured as well.")
    return CLOptions{..}

-- | Get command-line options.
getOptions :: IO CLOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool for Wallet Web API benchmarking."
