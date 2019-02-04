{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck (Property, Result (..), conjoin,
                     counterexample, ioProperty, label, property,
                     quickCheckResult, withMaxSuccess, (===))

import           Cardano.X509.Configuration (CertDescription (..),
                     DirConfiguration (..), ErrInvalidExpiryDays,
                     ServerConfiguration (..), TLSConfiguration (..),
                     fromConfiguration, genCertificate)
import           Data.X509.Extra (FailedReason, ServiceID, SignedCertificate,
                     genRSA256KeyPair, isServerCertificate,
                     validateCertificate)
import           Test.Cardano.X509.Configuration (tests)
import           Test.Cardano.X509.Configuration.Arbitrary (AltNames (..),
                     Invalid (..), Unknown (..))
import           Test.Pos.Util.Tripping (runTests)

--
-- Main
--

main :: IO ()
main = do
  runTests [ tests ]
  runQuickCheck
    [ quickCheckResult $ label "GenCertificate is Valid" propGenCertificateValid
    , quickCheckResult $ label "validateCertificate fails for unknown ServiceID" propUnknownService
    , quickCheckResult $ label "Invalid Expiry Days throws" propInvalidExpiryDays
    ]
  where
    -- NOTE running 'quickCheck prop' doesn't make 'cabal test' fails
    -- even if the property fails. So this little one cope with this
    -- by running all specs and failing if one of them returned a failure.
    runQuickCheck :: [IO Result] -> IO ()
    runQuickCheck =
        sequence >=> (mapM_ $ \case
            Success {} -> return ()
            _          -> exitFailure)


--
-- Properties
--

-- | Verify that each certificate generated is valid. Is uses the default
-- validation check of 'Data.X509.Validation'
propGenCertificateValid
    :: (TLSConfiguration, DirConfiguration)
    -> Property
propGenCertificateValid =
    ioProperty . generateAndValidate getValidServiceID propAllCertsValid


-- | Verify that each server certificate generated is invalid when provided an
-- unknown ServiceID.
propUnknownService
    :: Unknown AltNames
    -> (TLSConfiguration, DirConfiguration)
    -> Property
propUnknownService altNames =
    ioProperty . generateAndValidate (getUnknownServiceID altNames) propServerCertsInvalid


-- | Verify that we can't generate certificates when provided invalid
-- expiry days.
propInvalidExpiryDays
    :: (Invalid TLSConfiguration, DirConfiguration)
    -> Property
propInvalidExpiryDays (Invalid tlsConf, dirConf) =
    withMaxSuccess 10 $ ioProperty $ generateAndValidate getValidServiceID propAllCertsValid (tlsConf, dirConf)
        `catch` (\(_ :: ErrInvalidExpiryDays) -> return $ property True)
        `catch` (\(e :: SomeException)        -> throwM e)


-- | Check that there's no validation FailedReason
propAllCertsValid
    :: SignedCertificate
    -> [FailedReason]
    -> Property
propAllCertsValid _ =
    (=== [])


-- | Check that there are actually some validation FailedReason for non-client
-- certificate
propServerCertsInvalid
    :: SignedCertificate
    -> [FailedReason]
    -> Property
propServerCertsInvalid cert | isServerCertificate cert = (=/=) []
propServerCertsInvalid _    = const (property True)


-- | Actually generate certificates and validate them with the given property
-- Throws on error.
generateAndValidate
    :: (TLSConfiguration -> ServiceID)
    -> (SignedCertificate -> [FailedReason] -> Property)
    -> (TLSConfiguration, DirConfiguration)
    -> IO (Property)
generateAndValidate getServiceID predicate (tlsConf, dirConf) = do
    (caDesc, certDescs) <-
        fromConfiguration tlsConf dirConf genRSA256KeyPair <$> genRSA256KeyPair

    (_, caCert) <- genCertificate caDesc

    fmap conjoin $ forM certDescs $ \desc -> do
        (_, cert) <- genCertificate desc
        predicate cert <$>
            validateCertificate caCert (certChecks desc) (getServiceID tlsConf) cert


-- | Get a valid serviceID from the configuration
getValidServiceID
    :: TLSConfiguration
    -> ServiceID
getValidServiceID tlsConf =
    (NonEmpty.head $ serverAltNames $ tlsServer tlsConf, "")


-- | Get an invalid serviceID from the configuration
getUnknownServiceID
    :: Unknown AltNames
    -> TLSConfiguration
    -> ServiceID
getUnknownServiceID (Unknown (AltNames (name :| _))) _ =
    (name, "")


-- | Like '/=', but prints a counterexample when it fails.
-- Source: QuickCheck@2.12 Test.QuickCheck.Property#(=/=)
infix 4 =/=
(=/=) :: (Eq a, Show a) => a -> a -> Property
x =/= y =
  counterexample (show x ++ interpret res ++ show y) res
  where
    res = x /= y
    interpret True  = " /= "
    interpret False = " == "
