--
-- | Generate X.509 Certificates for TLS Client/Server authentication
--
module Main where

import           Control.Monad (forM_, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
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

data Command = Command
    { outDirectories :: DirConfiguration -- ^ Output directories configuration
    , configKey      :: ConfigurationKey -- ^ Configuration key within the config file
    , configFile     :: FilePath         -- ^ External Config file
    }

main :: IO ()
main = runOrFail $ do
    Command{..} <-
        lift $ execParser opts

    tlsConfig <-
        decodeEitherConfigFile configKey configFile

    (caDesc, descs) <-
        fromConfiguration tlsConfig outDirectories genRSA256KeyPair <$> lift genRSA256KeyPair

    let caName =
            certFilename caDesc

    let serverHost = -- NOTE We expect at least one alternative name
            (head $ serverAltNames $ tlsServer tlsConfig, "")

    (caKey, caCert) <-
        genCertificate caDesc

    case certOutDir caDesc of
        Nothing  -> return ()
        Just dir -> lift $ writeCertificate (dir </> caName) (caKey, caCert)

    forM_ descs $ \desc@CertDescription{..} -> do
        (key, cert) <- genCertificate desc
        validateSHA256 caCert certChecks serverHost cert
        lift $ writeCertificate  (certOutDir </> certFilename) (key, cert)
        lift $ writeCertificate_ (certOutDir </> caName)       caCert
  where
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

        <*> strOption (short 'k' <> long "configuration-key" <> metavar "KEY"
            <> help "Configuration key within the config file (e.g. 'dev' or 'test')")

        <*> strOption (short 'c' <> long "configuration-file" <> metavar "FILEPATH"
            <> help "Configuration file describing the PKI")


-- | Run an ExceptT and simply fail on error
runOrFail :: Monad m => ExceptT String m a -> m a
runOrFail =
    runExceptT >=> either fail return

-- | Generate & sign a certificate from a certificate description
genCertificate
    :: CertDescription IO PublicKey PrivateKey filename
    -> ExceptT String IO (PrivateKey, SignedCertificate)
genCertificate CertDescription{..} = do
    ((pub, priv), now) <- lift $ (,) <$> certGenKeys <*> dateCurrent

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
