-- | This module tests Binary instances.

module Test.Pos.Types.Identity.TimestampSpec
       ( spec
       ) where

import           Pos.Types             (Timestamp)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Test.Pos.Util         (binaryEncodeDecode, showRead)

spec :: Spec
spec = describe "Timestamp" $ do
    describe "Identity testing" $ do
        prop "Bi" (binaryEncodeDecode @Timestamp)
        prop "Show/Read" (showRead @Timestamp)
