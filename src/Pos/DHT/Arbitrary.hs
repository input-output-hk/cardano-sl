-- | Arbitrary instances for DHT types

module Pos.DHT.Arbitrary () where

import           Test.QuickCheck (Arbitrary (..), oneof)
import           Universum

import           Pos.DHT.Model    (DHTMsgHeader (..), DHTData (..), DHTKey (..))

instance Arbitrary DHTMsgHeader where
    arbitrary = oneof [ pure BroadcastHeader
                      , SimpleHeader <$> arbitrary
                      ]

instance Arbitrary DHTData where
    arbitrary = DHTData <$> arbitrary

instance Arbitrary DHTKey where
    arbitrary = DHTKey <$> arbitrary
