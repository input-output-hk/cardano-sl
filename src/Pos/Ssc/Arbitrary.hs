module Pos.Ssc.Arbitrary
       ( SscPayloadDependsOnSlot (..)
       ) where

import           Test.QuickCheck     (Arbitrary (..), Gen)
import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Ssc.NistBeacon  (SscNistBeacon)
import           Pos.Types           (SlotId)

newtype SscPayloadDependsOnSlot ssc = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen (SscPayload ssc)
    }

instance Arbitrary (SscPayloadDependsOnSlot SscNistBeacon) where
    arbitrary = pure $ SscPayloadDependsOnSlot (\_ -> arbitrary)
