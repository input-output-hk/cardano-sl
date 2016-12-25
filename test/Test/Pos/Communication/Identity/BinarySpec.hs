-- | This module tests Binary instances for Pos.Communication types

module Test.Pos.Communication.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Communication     as C

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "Communication" $ do
    describe "Bi instances" $ do
        prop "SysStartRequest"          (binaryEncodeDecode @C.SysStartRequest)
        prop "SysStartResponse"         (binaryEncodeDecode @C.SysStartResponse)
        prop "SendProxySK"              (binaryEncodeDecode @C.SendProxySK)
        prop "ConfirmProxySK"           (binaryEncodeDecode @C.ConfirmProxySK)
        prop "CheckProxySKConfirmed"    (binaryEncodeDecode @C.CheckProxySKConfirmed)
        prop "CheckProxySKConfirmedRes" (binaryEncodeDecode @C.CheckProxySKConfirmedRes)
