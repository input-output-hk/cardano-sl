{-# LANGUAGE TypeApplications #-}

-- | This module tests Binary instances.

module Test.Pos.Types.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Pos.Types             (Tx)

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "Types" $ do
    describe "Binary instances" $ do
        prop
            "Tx"
            (binaryEncodeDecode @Tx)
