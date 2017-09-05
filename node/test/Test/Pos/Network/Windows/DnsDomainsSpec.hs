{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | Specification of 'Pos.Network.Windows.DnsDomains'

module Test.Pos.Network.Windows.DnsDomainsSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                     (Spec)
#if !defined(POSIX)
import           Network.URI                    (isIPv4address)
import           Pos.Network.Types              (initDnsOnUse)
import           Test.Hspec                     (Expectation, describe, it, shouldBe,
                                                 shouldSatisfy)
import           Test.Hspec.QuickCheck          (prop)

import qualified Pos.Network.Windows.DnsDomains as Win
#endif

spec :: Spec
#if defined(POSIX)
spec = return ()
#else
spec = do
    describe "Default DNS server on Windows" $ do
        describe "getWindowsDefDnsServer" $ do
            prop "Successfully retrieves this Windows machine's DNS server"
                 prop_GetWindowsDefDnsServer
    describe "Multi-value lookups" $ do
        it "pool.ntp.org resolves correctly to more than 1 address" $ testLookupFor "pool.ntp.org"

prop_GetWindowsDefDnsServer :: Expectation
prop_GetWindowsDefDnsServer = do
    Win.getWindowsDefaultDnsServer >>= \dnsIpAddr ->
        dnsIpAddr `shouldSatisfy` (maybe False isIPv4address)

testLookupFor :: ByteString -> Expectation
testLookupFor dnsDomain = do
    initDnsOnUse $ \resolve -> do
      res <- resolve dnsDomain
      (> 1) . length <$> res `shouldBe` Right True
#endif
