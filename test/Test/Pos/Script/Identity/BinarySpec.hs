-- | This module tests Binary instances.

module Test.Pos.Script.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Script             as S

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "Script" $ do
    describe "Bi instances" $ do
        prop "Script" (binaryEncodeDecode @S.Script)
