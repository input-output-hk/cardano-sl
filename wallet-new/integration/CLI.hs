{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with command-line options.

module CLI
    ( CLOptions (..)
    , getOptions
    ) where

import           Universum

import           Options.Applicative -- (Parser, execParser, fullDesc, header, help, helper, info,
                                      --long, metavar, progDesc, strOption, switch)

-- | Parser for command-line options.
optionsParser :: Parser CLOptions
optionsParser = do
    tlsPubCertPath <- pure "./scripts/tls-files/server.crt"
--        <- strOption $
--           long        "tls-pub-cert"
--        <> metavar     "PUB_CERT"
--        <> help        "Path to TLS public certificate"
    tlsPrivKeyPath <- pure "./scripts/tls-files/server.key"
--        <- strOption $
--           long        "tls-priv-key"
--        <> metavar     "PRIV_KEY"
--        <> help        "Path to TLS private key"
    stateless
        <- switch $
           long        "stateless"
        <> help        "If defined - don't persist state between requests."

    return CLOptions{..}


-- | Get command-line options.
getOptions :: IO CLOptions
getOptions = execParser programInfo
  where
    programInfo = info (helper <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool for Wallet Web integration testing."


-- | The configuration for the application.
data CLOptions = CLOptions
    { tlsPubCertPath :: FilePath
    , tlsPrivKeyPath :: FilePath
    , stateless      :: Bool
    } deriving (Show, Eq)
