-- | This module tests Show/Read instances.

module Test.Pos.Types.Identity.ShowReadSpec
       ( spec
       ) where

import           Universum

import           Pos.Core (Timestamp (..))
import           Test.Hspec (Spec, describe)

import           Test.Pos.Binary.Helpers (showReadTest)
import           Test.Pos.Core.Arbitrary ()

spec :: Spec
spec = describe "Types" $ do
    describe "Show/Read instances" $ do
        showReadTest @Timestamp
