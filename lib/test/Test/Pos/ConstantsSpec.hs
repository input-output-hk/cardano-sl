-- | This module tests some invariants on constants.

module Test.Pos.ConstantsSpec
       ( spec
       ) where

import           Universum

import           Pos.Chain.Update (HasUpdateConfiguration, SystemTag (..),
                     ourSystemTag, updateConfiguration)

import           Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)
import           Test.Pos.Configuration (withDefUpdateConfiguration)

-- | @currentSystemTag@ is a value obtained at compile time with TemplateHaskell
-- that represents that current system's platform (i.e. where it was compiled).
-- As of the @cardano-sl-1.0.4@, the only officially supported systems are @win64@ and
-- @macos64@ (@linux64@ can be built and used from source).
-- If @currentSystemTag@ is not one of these two when this test is ran with
-- @cardano-sl-1.0.4@, something has gone wrong.
systemTagCheck :: HasUpdateConfiguration => Expectation
systemTagCheck = do
    let sysTags = map SystemTag ["linux64", "macos64", "win64"]
        felem = flip elem
    ourSystemTag updateConfiguration `shouldSatisfy` felem sysTags

spec :: Spec
spec = withDefUpdateConfiguration $ describe "Constants" $ do
    describe "Configuration constants" $ do
        it "currentSystemTag" $ systemTagCheck
