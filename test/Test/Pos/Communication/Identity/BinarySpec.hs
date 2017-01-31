-- | This module tests Binary instances for Pos.Communication types

module Test.Pos.Communication.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec        (Spec, describe)
import           Universum

import qualified Pos.Communication as C
import qualified Pos.Delegation    as D

import           Test.Pos.Util     (networkBinaryTest)

spec :: Spec
spec = describe "Communication" $ do
    describe "Bi instances" $ do
        networkBinaryTest @C.SysStartRequest
        networkBinaryTest @C.SysStartResponse
        networkBinaryTest @C.HandlerSpec
        networkBinaryTest @C.VerInfo
        networkBinaryTest @D.SendProxySK
        networkBinaryTest @D.ConfirmProxySK
        --binaryTest @D.CheckProxySKConfirmed
        --binaryTest @D.CheckProxySKConfirmedRes
