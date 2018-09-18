-- | Generate X.509 Certificates for TLS Client/Server authentication

module Main where

import           Universum

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, execParser, fullDesc, header,
                     help, helper, info, long, metavar, progDesc, short,
                     strOption, (<**>))
import           System.FilePath.Posix (FilePath, (</>))

import           Cardano.X509.Configuration (CertDescription (..),
                     ConfigurationKey (..), DirConfiguration (..),
                     ServerConfiguration (..), TLSConfiguration (..),
                     decodeConfigFile, fromConfiguration, genCertificate)
import           Data.X509.Extra (failIfReasons, genRSA256KeyPair,
                     validateSHA256, writeCertificate, writeCredentials)


data Command = Command
    { outDirectories :: DirConfiguration -- ^ Output directories configuration
    , configKey      :: ConfigurationKey -- ^ Configuration key within the config file
    , configFile     :: FilePath         -- ^ External Config file
    }


-- | opt-applicative Parser for the 'Command' above
cmdParser :: Parser Command
cmdParser = Command
    <$> (DirConfiguration
        <$> strOption (long "server-out-dir" <> metavar "FILEPATH"
            <> help "Output directory for Server certificate & private key")

        <*> strOption (long "clients-out-dir" <> metavar "FILEPATH"
            <> help "Output directory for Client certificate(s) & private key")

        <*> optional (strOption (long "ca-out-dir" <> metavar "FILEPATH"
            <> help "Optional, output directory for the CA certificate & private key"))
    )

    <*> (ConfigurationKey
        <$> strOption (short 'k' <> long "configuration-key" <> metavar "KEY"
            <> help "Configuration key within the config file (e.g. 'dev' or 'test')")
    )

    <*> strOption (short 'c' <> long "configuration-file" <> metavar "FILEPATH"
        <> help "Configuration file describing the PKI")


main :: IO ()
main = do
    cmd <- execParser $
        info (cmdParser <**> helper)
        (  fullDesc
        <> header "X.509 Certificates Generation"
        <> progDesc "Pure Haskell 'replacement' for OpenSSL to generate certificates for a TLS Private Key Infrastructure"
        )

    tlsConfig <-
        decodeConfigFile (configKey cmd) (configFile cmd)

    (caDesc, descs) <-
        fromConfiguration tlsConfig (outDirectories cmd) genRSA256KeyPair <$> genRSA256KeyPair

    let caName =
            certFilename caDesc

    let (serverHost, serverPort) =
            (NonEmpty.head $ serverAltNames $ tlsServer tlsConfig, "")

    (caKey, caCert) <-
        genCertificate caDesc

    case certOutDir caDesc of
        Nothing  -> return ()
        Just dir -> writeCredentials (dir </> caName) (caKey, caCert)

    forM_ descs $ \desc -> do
        (key, cert) <- genCertificate desc
        failIfReasons =<< validateSHA256
            caCert
            (certChecks desc)
            (serverHost, serverPort)
            cert
        writeCredentials (certOutDir desc </> certFilename desc) (key, cert)
        writeCertificate (certOutDir desc </> caName) caCert
