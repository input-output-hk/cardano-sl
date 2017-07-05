-- | This module tests Binary instances for 'Pos.Delegation' types.

module Test.Pos.Delegation.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                       (Spec, describe)

import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Delegation                   (DlgPayload, ProxySKLightConfirmation)
import           Pos.Infra.Arbitrary              ()
import           Test.Pos.Util                    (binaryTest, networkBinaryTest)


spec :: Spec
spec = describe "Delegation types" $ do
    describe "Bi instances" $ do
        binaryTest @DlgPayload
        binaryTest @(DataMsg ProxySKLightConfirmation)
    describe "Network" $ do
        networkBinaryTest @(DataMsg ProxySKLightConfirmation)
