-- | This module tests Binary instances.

module Test.Pos.DHT.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.DHT               as DHT

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "DHT.Model" $ do
    describe "Bi instances" $ do
        prop "DHTMsgHeader" (binaryEncodeDecode @DHT.DHTMsgHeader)
        prop "DHTKey"       (binaryEncodeDecode @DHT.DHTKey)
        prop "DHTData"      (binaryEncodeDecode @DHT.DHTData)
