-- | This module tests Binary instances.

module Test.Pos.Types.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.Util.BackupPhrase (BackupPhrase)

import           Test.Pos.Helpers (binaryTest)
import           Test.Pos.Util (withDefConfiguration, withDefInfraConfiguration)

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $ describe "Types" $ do
    describe "Bi instances" $ do
        describe "Util" $ do
            binaryTest @BackupPhrase
