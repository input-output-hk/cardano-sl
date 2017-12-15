-- | This module tests Binary instances for 'Pos.Explorer' types.

module Test.Pos.Explorer.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.Explorer ()

spec :: Spec
spec = describe "Explorer types" $ do
    pass
    -- TODO uncomment this code when
    -- @binaryTest@ will be in lib/src/Test
    -- describe "Bi instances" $ do
    --     binaryTest @TxExtra
