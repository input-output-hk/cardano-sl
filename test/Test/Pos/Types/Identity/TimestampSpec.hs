{-# LANGUAGE TypeApplications #-}

-- | This module tests Binary instances.

module Test.Pos.Types.Identity.TimestampSpec
       ( spec
       ) where

import           Pos.Types             (Timestamp)
import           Test.Hspec            (Spec, describe)
import           Universum

import           Test.Pos.Util         (binaryTest, showReadTest)

spec :: Spec
spec = describe "Timestamp" $ do
    describe "Identity testing" $ do
        describe "Bi instance" $ do
            binaryTest @Timestamp
        describe "Show/Read instances" $ do
            showReadTest @Timestamp
