{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with command-line options.

module CLI
    ( CLIOptions (..)
    , getOptions
    ) where

import           Data.String (words)
import           Options.Applicative
import           Universum

-- | Parser for command-line options.
optionsParser :: Parser CLIOptions
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

    hSpecOptions <- fmap Data.String.words $ strOption $
        long        "hspec-options"
        <> metavar  "OPTIONS"
        <> value    ""
        <> help     "extra options to pass to HSpec"
        <> showDefault

    pure CLIOptions{..}


-- | Get command-line options.
getOptions :: IO CLIOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool for Wallet Web integration testing."


-- | The configuration for the application.
data CLIOptions = CLIOptions
    { tlsClientCertPath :: FilePath
    , tlsPrivKeyPath    :: FilePath
    , tlsCACertPath     :: FilePath
    , serverHost        :: String
    , serverPort        :: Int
    , hSpecOptions      :: [String]
    } deriving (Show, Eq)
