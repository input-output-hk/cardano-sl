module Pos.Ssc.Arbitrary
       ( SscPayloadDependsOnSlot (..)
       ) where

import           Universum

import           Test.QuickCheck          (Arbitrary (..), Gen)

import           Pos.Ssc.Class.Types      (Ssc (..))
import           Pos.Ssc.NistBeacon       (SscNistBeacon)
import           Pos.Types                (SlotId)

newtype SscPayloadDependsOnSlot ssc = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen (SscPayload ssc)
    }

instance Arbitrary (SscPayloadDependsOnSlot SscNistBeacon) where
    arbitrary = pure $ SscPayloadDependsOnSlot (\_ -> arbitrary)
