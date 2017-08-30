{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Pos.Network.Types              (initDnsOnUse)
import           Test.Hspec                     (Expectation, Spec, describe, it,
                                                 shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck          (prop)

#if !defined(POSIX)
import qualified Pos.Network.Windows.DnsDomains as Win
#endif


testLookupFor :: ByteString -> Expectation
testLookupFor dnsDomain = do
    initDnsOnUse $ \resolve -> do
      res <- resolve dnsDomain
      isRight res `shouldBe` True

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
        it "www.google.it resolves correctly" $ testLookupFor "www.google.it"
        it "cardano-node-0.aws.iohkdev.io resolves correctly" $ testLookupFor "cardano-node-0.aws.iohkdev.io"
        it "cardano-node-1.aws.iohkdev.io resolves correctly" $ testLookupFor "cardano-node-1.aws.iohkdev.io"

prop_GetWindowsDefDnsServer :: Expectation
prop_GetWindowsDefDnsServer = do
    Win.getWindowsDefaultDnsServer >>= \dnsIpAddr ->
        dnsIpAddr `shouldSatisfy` (maybe False isIPv4address)
#endif
