--
-- | Generate X.509 Certificates for TLS Client/Server authentication
--
module Main where

import           Universum

import           Crypto.PubKey.RSA (PrivateKey, PublicKey)
import           Data.Hourglass (Minutes (..), Period (..), dateAddPeriod, timeAdd)
import           Data.Semigroup ((<>))
import           Data.X509 (Certificate (..), Extensions (..), PubKey (PubKeyRSA),
                            SignedCertificate, encodeSignedObject)
import           Options.Applicative
import           System.FilePath.Posix (FilePath, (</>))
import           Time.System (dateCurrent)
import           Time.Types (DateTime (..))

import           Configuration
import           Data.X509.Extra

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty


data Command = Command
    { outDirectories :: DirConfiguration -- ^ Output directories configuration
    , configKey      :: ConfigurationKey -- ^ Configuration key within the config file
    , configFile     :: FilePath         -- ^ External Config file
    }

main :: IO ()
main =
    let
        opts :: ParserInfo Command
        opts =
            info (cmdParser <**> helper)
                ( fullDesc
                <> header "X.509 Certificates Generation"
                <> progDesc "Pure Haskell 'replacement' for OpenSSL to generate certificates for a TLS Private Key Infrastructure"
                )

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
    in do
        Command{..} <-
            execParser opts

        tlsConfig <-
            decodeConfigFile configKey configFile

        (caDesc, descs) <-
            fromConfiguration tlsConfig outDirectories genRSA256KeyPair <$> genRSA256KeyPair

        let caName =
                certFilename caDesc

        let (serverHost, serverPort) = -- NOTE We expect at least one alternative name
                (NonEmpty.head $ serverAltNames $ tlsServer tlsConfig, "")

        (caKey, caCert) <-
            genCertificate caDesc

        case certOutDir caDesc of
            Nothing  -> return ()
            Just dir -> writeCertificate (dir </> caName) (caKey, caCert)

        forM_ descs $ \desc@CertDescription{..} -> do
            (key, cert) <- genCertificate desc
            validateSHA256 caCert certChecks (serverHost, serverPort) cert >>= failIfReasons
            writeCertificate  (certOutDir </> certFilename) (key, cert)
            writeCertificate_ (certOutDir </> caName)       caCert


-- | Generate & sign a certificate from a certificate description
genCertificate
    :: CertDescription IO PublicKey PrivateKey filename
    -> IO (PrivateKey, SignedCertificate)
genCertificate CertDescription{..} = do
    ((pub, priv), now) <- (,) <$> certGenKeys <*> dateCurrent

    let CertConfiguration{..} = certConfiguration
    let cert = Certificate
            { certVersion      = 2
            , certSerial       = fromIntegral certSerial
            , certSignatureAlg = signAlgRSA256
            , certValidity     = (addMinutes (-1) now, addDays certExpiryDays now)
            , certPubKey       = PubKeyRSA pub
            , certExtensions   = Extensions (Just certExtensions)
            , certIssuerDN     = certIssuer
            , certSubjectDN    = certSubject
            }

    (priv,) <$> signCertificate certSigningKey cert
  where
    addDays :: Int -> DateTime -> DateTime
    addDays n time@DateTime{..} =
        time { dtDate = dateAddPeriod dtDate (mempty { periodDays = n }) }

    addMinutes :: Int -> DateTime -> DateTime
    addMinutes n time =
        timeAdd time (Minutes $ fromIntegral n)


-- | Write a certificate and its private key to the given location
writeCertificate
    :: FilePath
    -> (PrivateKey, SignedCertificate)
    -> IO ()
writeCertificate filename (key, cert) = do
    BS.writeFile (filename <> ".key") (encodeRSAPrivateKey key)
    writeCertificate_ filename cert


-- | Write a certificate to the given location
writeCertificate_
    :: FilePath
    -> SignedCertificate
    -> IO ()
writeCertificate_ filename cert =
    BS.writeFile (filename <> ".crt") (encodeSignedObject cert)
