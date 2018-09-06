-- | Tests for canonical serialization of genesis data.

module Test.Pos.Genesis.CanonicalSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (arbitrary, generate)

import           Pos.Core.Genesis (GenesisAvvmBalances, GenesisData, GenesisDelegation,
                                   GenesisProtocolConstants, GenesisWStakeholders)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Helpers (canonicalJsonTest)

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ describe "Genesis" $ modifyMaxSuccess (const 10) $ do
    describe "Canonical encoding" $ do
        canonicalJsonTest @GenesisProtocolConstants
        canonicalJsonTest @GenesisAvvmBalances
        canonicalJsonTest @GenesisWStakeholders
        canonicalJsonTest @GenesisDelegation
        canonicalJsonTest @GenesisData
