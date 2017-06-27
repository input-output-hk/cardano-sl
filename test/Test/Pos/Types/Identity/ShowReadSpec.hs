-- | This module tests Show/Read instances.

module Test.Pos.Types.Identity.ShowReadSpec
       ( spec
       ) where

import           Universum

import qualified Pos.Types     as T (Timestamp)
import           Test.Hspec         (Spec, describe)

import           Test.Pos.Util (showReadTest)

spec :: Spec
spec = describe "Types" $ do
    describe "Show/Read instances" $ do
        showReadTest @T.Timestamp
