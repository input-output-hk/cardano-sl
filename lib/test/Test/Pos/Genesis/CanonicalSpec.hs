-- | Tests for canonical serialization of genesis data.

module Test.Pos.Genesis.CanonicalSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Arbitrary.Core ()
import           Pos.Core.Genesis (GenesisAvvmBalances, GenesisData, GenesisDelegation,
                                   GenesisWStakeholders, ProtocolConstants)

import           Test.Pos.Helpers (canonicalJsonTest)
import           Test.Pos.Util (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Genesis" $ modifyMaxSuccess (const 10) $ do
    describe "Canonical encoding" $ do
        canonicalJsonTest @ProtocolConstants
        canonicalJsonTest @GenesisAvvmBalances
        canonicalJsonTest @GenesisWStakeholders
        canonicalJsonTest @GenesisDelegation
        canonicalJsonTest @GenesisData
