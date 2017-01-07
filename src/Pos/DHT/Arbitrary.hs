{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for DHT types

module Pos.DHT.Arbitrary () where

import           Data.DeriveTH               (derive, makeArbitrary)
import           Network.Kademlia.HashNodeId (HashId (..))
import           Test.QuickCheck             (Arbitrary (..), oneof)
import           Universum

import           Pos.DHT.Model               (DHTData (..), DHTKey (..),
                                              DHTMsgHeader (..))

instance Arbitrary DHTMsgHeader where
    arbitrary = oneof [ pure BroadcastHeader
                      , SimpleHeader <$> arbitrary
                      ]

instance Arbitrary DHTKey where
    arbitrary = DHTKey . HashId <$> arbitrary

derive makeArbitrary ''DHTData
