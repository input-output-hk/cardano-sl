{-# LANGUAGE TypeApplications #-}

-- | This module tests Binary instances.

module Test.Pos.Types.Identity.TimestampSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Types             (Timestamp)

import           Test.Pos.Util         (binaryEncodeDecode, msgPackEncodeDecode,
                                        safeCopyEncodeDecode, showRead)

spec :: Spec
spec = describe "Timestamp" $ do
    describe "Identity testing" $ do
        prop "Binary" (binaryEncodeDecode @Timestamp)
        prop "MessagePack" (msgPackEncodeDecode @Timestamp)
        prop "Show/Read" (showRead @Timestamp)
