-- | This module tests Binary instances.

module Test.Pos.Script.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.Script    as S

import           Test.Pos.Util (binaryTest)

spec :: Spec
spec = describe "Script" $ do
    describe "Bi instances" $ do
        binaryTest @S.Script
