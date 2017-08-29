{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | Specification of 'Pos.Network.Windows.DnsDomains'

module Test.Pos.Network.Windows.DnsDomainsSpec
       (
         spec
       ) where

import           Universum

import           Network.URI                    (isIPv4address)
import           Test.Hspec                     (Expectation, Spec, describe,
                                                 shouldSatisfy)
import           Test.Hspec.QuickCheck          (prop)

#if !defined(POSIX)
import qualified Pos.Network.Windows.DnsDomains as Win
#endif

spec :: Spec
#if defined(POSIX)
spec = return ()
#else
spec =
    describe "Default DNS server on Windows" $ do
        describe "getWindowsDefDnsServer" $ do
            prop "Successfully retrieves this Windows machine's DNS server"
                 prop_GetWindowsDefDnsServer

prop_GetWindowsDefDnsServer :: Expectation
prop_GetWindowsDefDnsServer = do
    Win.getWindowsDefaultDnsServer >>= \dnsIpAddr ->
        dnsIpAddr `shouldSatisfy` (maybe False isIPv4address)
#endif
