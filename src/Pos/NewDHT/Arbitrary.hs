{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for DHT types

module Pos.NewDHT.Arbitrary () where

import           Data.DeriveTH   (derive, makeArbitrary)
import           Test.QuickCheck (Arbitrary (..), oneof)
import           Universum

--import           Pos.NewDHT.Model    (DHTMsgHeader (..), DHTData (..), DHTKey (..))

--instance Arbitrary DHTMsgHeader where
--    arbitrary = oneof [ pure BroadcastHeader
--                      , SimpleHeader <$> arbitrary
--                      ]

--derive makeArbitrary ''DHTData
--derive makeArbitrary ''DHTKey
