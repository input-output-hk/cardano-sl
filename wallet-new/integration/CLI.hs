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

    serverPort0 <- option auto $
        long        "server-port"
       <> metavar   "PORT0"
       <> value     8090
       <> help      "Server port"
       <> showDefault

    serverPort1 <- option auto $
        long        "server-port"
       <> metavar   "PORT1"
       <> value     8091
       <> help      "Server port"
       <> showDefault

    serverPort2 <- option auto $
        long        "server-port"
       <> metavar   "PORT2"
       <> value     8092
       <> help      "Server port"
       <> showDefault

    serverPort3 <- option auto $
        long        "server-port"
       <> metavar   "PORT3"
       <> value     8093
       <> help      "Server port"
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
    , serverPort0       :: Int
    , serverPort1       :: Int
    , serverPort2       :: Int
    , serverPort3       :: Int
    } deriving (Show, Eq)
