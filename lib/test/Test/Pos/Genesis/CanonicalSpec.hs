-- | Tests for canonical serialization of genesis data.

module Test.Pos.Genesis.CanonicalSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Core.Genesis (GenesisAvvmBalances, GenesisData,
                     GenesisDelegation, GenesisProtocolConstants,
                     GenesisWStakeholders)

import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Helpers (canonicalJsonTest)

spec :: Spec
spec = withDefConfiguration $ \_ -> describe "Genesis" $ modifyMaxSuccess (const 10) $ do
    describe "Canonical encoding" $ do
        canonicalJsonTest @GenesisProtocolConstants
        canonicalJsonTest @GenesisAvvmBalances
        canonicalJsonTest @GenesisWStakeholders
        canonicalJsonTest @GenesisDelegation
        canonicalJsonTest @GenesisData
