-- | Specification of 'Pos.Types.SharedSeed' type

module Test.Pos.Types.SeedSpec
       (spec
       ) where

import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.Pos.Util         (formsCommutativeMonoid)
import           Test.QuickCheck       (Property, (.&&.))

import           Pos.Types             (SharedSeed)

spec :: Spec
spec = describe "SharedSeed" $ do
    prop description_xorFormsAbelianGroup xorFormsAbelianGroup
  where
    description_xorFormsAbelianGroup =
        "under the xor operation, the set of ftsSeedLength-byte SharedSeeds is an abelian\
        \ group"

xorFormsAbelianGroup :: SharedSeed -> SharedSeed -> SharedSeed -> Property
xorFormsAbelianGroup fts1 fts2 fts3 =
    let hasInverses =
            let inv1 = fts1 <> fts2
                inv2 = inv1 <> fts2
                inv3 = fts1 <> inv1
            in inv2 == fts1 && inv3 == fts2
    in formsCommutativeMonoid fts1 fts2 fts3 .&&. hasInverses
