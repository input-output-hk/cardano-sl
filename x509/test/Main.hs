module Main where

import           Universum

import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck (Property, conjoin, ioProperty, quickCheck,
                     withMaxSuccess, (===))

import           Cardano.X509.Configuration (CertDescription (..),
                     DirConfiguration (..), ServerConfiguration (..),
                     TLSConfiguration (..), fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair, validateSHA256)
import           Test.Cardano.X509.Configuration.Arbitrary ()


main :: IO ()
main = quickCheck $
    propGenCertificateValid


propGenCertificateValid :: (TLSConfiguration, DirConfiguration) -> Property
propGenCertificateValid (tlsConf, dirConf) = ioProperty $ do
    (caDesc, certDescs) <-
        fromConfiguration tlsConf dirConf genRSA256KeyPair <$> genRSA256KeyPair

    (_, caCert) <- genCertificate caDesc

    let serverId = (NonEmpty.head $ serverAltNames $ tlsServer tlsConf, "")

    fmap conjoin $ forM certDescs $ \desc -> do
        (_, cert) <- genCertificate desc

        errors <-
            validateSHA256 caCert (certChecks desc) serverId cert

        return (errors === [])
