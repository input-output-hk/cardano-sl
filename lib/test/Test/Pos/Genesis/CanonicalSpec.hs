-- | Tests for canonical serialization of genesis data.

module Test.Pos.Genesis.CanonicalSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Chain.Genesis (GenesisAvvmBalances, GenesisDelegation,
                     GenesisWStakeholders)
import           Pos.Crypto (RequiresNetworkMagic (..))

import           Test.Pos.Chain.Genesis.Arbitrary (genGenesisData,
                     genGenesisProtocolConstants)
import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)
import           Test.Pos.Helpers (canonicalJsonTest, canonicalJsonTestWithGen)

spec :: Spec
spec = describe "Genesis" $ modifyMaxSuccess (const 10) $ do
    describe "Canonical encoding" $ do
        -- Restricted canonical JSON identity tests for those types which
        -- include `ProtocolMagic`.
        --
        -- This must be done since the canonical `ToJSON` instance of
        -- `ProtocolMagic` does not output the `RequiresNetworkMagic` field
        -- and the canonical `FromJSON` instance defaults its value to
        -- `RequiresMagic`.
        describe "Generator restricted to only use RequiresMagic" $ do
            let genPM = genProtocolMagicUniformWithRNM RequiresMagic
            canonicalJsonTestWithGen $ genGenesisProtocolConstants genPM
            canonicalJsonTestWithGen $ genGenesisData (genGenesisProtocolConstants genPM)
         -- Unrestricted canonical JSON identity tests
        describe "Unrestricted tests" $ do
            canonicalJsonTest @GenesisAvvmBalances
            canonicalJsonTest @GenesisWStakeholders
            canonicalJsonTest @GenesisDelegation
