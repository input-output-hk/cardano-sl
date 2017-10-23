-- | This module tests Binary instances for 'Pos.Delegation' types.

module Test.Pos.Delegation.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                    (Spec, describe)

import           Pos.Arbitrary.Delegation      ()
import           Pos.Arbitrary.Infra           ()
import           Pos.Communication.Types.Relay (DataMsg (..))
import           Pos.Core                      (ProxySKHeavy, ProxySKLight)
import           Pos.Delegation                (DlgPayload, ProxySKLightConfirmation)

import           Test.Pos.Helpers              (binaryTest)
import           Test.Pos.Util                 (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Delegation types" $ do
    describe "Bi instances" $ do
        binaryTest @DlgPayload
    describe "Network" $ do
        binaryTest @(DataMsg ProxySKLightConfirmation)
        binaryTest @(DataMsg ProxySKLight)
        binaryTest @(DataMsg ProxySKHeavy)
