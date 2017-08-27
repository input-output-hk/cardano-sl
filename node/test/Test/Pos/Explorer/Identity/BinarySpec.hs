-- | This module tests Binary instances for 'Pos.Explorer' types.

module Test.Pos.Explorer.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Explorer          (TxExtra)
import           Test.Pos.Util         (binaryTest)


spec :: Spec
spec = describe "Explorer types" $ do
    describe "Bi instances" $ do
        modifyMaxSuccess (const 100) $ binaryTest @TxExtra
