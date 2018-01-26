-- | This module tests Binary instances for Pos.Communication types

module Test.Pos.Communication.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Node.Message.Class (MessageCode)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Arbitrary.Infra ()
import qualified Pos.Communication as C

import           Test.Pos.Cbor.CborSpec (extensionProperty)
import           Test.Pos.Helpers (binaryTest)

spec :: Spec
spec = describe "Communication" $ do
    describe "Bi instances" $ do
        binaryTest @C.HandlerSpec
        binaryTest @C.VerInfo
        binaryTest @MessageCode
    describe "Bi extension" $ do
        prop "HandlerSpec" (extensionProperty @C.HandlerSpec)
