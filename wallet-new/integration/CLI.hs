{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with command-line options.

module CLI
    ( CLOptions (..)
    , getOptions
    ) where

import           Universum

import           Options.Applicative


-- | Parser for command-line options.
optionsParser :: Parser CLOptions
optionsParser = do
    tlsClientCertPath <- strOption $
        long        "tls-client-cert"
        <> metavar  "FILEPATH"
        <> help     "Path to TLS client public certificate"

    tlsPrivKeyPath <- strOption $
        long        "tls-key"
        <> metavar  "FILEPATH"
        <> help     "Path to TLS client private key"

    tlsCACertPath <- strOption $
        long        "tls-ca-cert"
        <> metavar  "FILEPATH"
        <> help     "Path to TLS CA public certificate"

    serverHost <- strOption $
        long        "server-host"
        <> metavar  "HOSTNAME"
        <> value    "localhost"
        <> help     "Server hostname"
        <> showDefault

    serverPort <- option auto $
        long        "server-port"
       <> metavar   "PORT"
       <> value     8090
       <> help      "Server port"
       <> showDefault

    testRunnerMatch <- optional . strOption $
        long        "match"
        <> metavar  "PATTERN"
        <> help     "Only tests that match PATTERN will be run"
        <> showDefault

    testRunnerSeed <- optional . option auto $
        long        "seed"
        <> metavar  "SEED"
        <> help     "Seed for a test runner"
        <> showDefault


    pure CLOptions{..}


-- | Get command-line options.
getOptions :: IO CLOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool for Wallet Web integration testing."


-- | The configuration for the application.
data CLOptions = CLOptions
    { tlsClientCertPath :: FilePath
    , tlsPrivKeyPath    :: FilePath
    , tlsCACertPath     :: FilePath
    , serverHost        :: String
    , serverPort        :: Int
    , testRunnerMatch   :: Maybe String
    , testRunnerSeed    :: Maybe Integer
    } deriving (Show, Eq)
