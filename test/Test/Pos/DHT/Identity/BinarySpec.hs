-- | This module tests Binary instances.

module Test.Pos.DHT.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.DHT       as DHT

import           Test.Pos.Util (binaryTest)

spec :: Spec
spec = describe "DHT.Model" $ do
    describe "Bi instances" $ do
        binaryTest @DHT.DHTMsgHeader
        binaryTest @DHT.DHTKey
        binaryTest @DHT.DHTData
