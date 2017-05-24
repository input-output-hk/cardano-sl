-- | This module tests Binary instances for Pos.Communication types

module Test.Pos.Communication.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec               (Spec, describe)
import           Universum

import qualified Pos.Communication        as C
--import qualified Pos.Delegation           as D

import           Test.Pos.Arbitrary.Infra ()
import           Test.Pos.Util            (networkBinaryTest)

spec :: Spec
spec = describe "Communication" $ do
    describe "Bi instances" $ do
        networkBinaryTest @C.HandlerSpec
        networkBinaryTest @C.VerInfo
        --binaryTest @D.CheckProxySKConfirmed
        --binaryTest @D.CheckProxySKConfirmedRes
