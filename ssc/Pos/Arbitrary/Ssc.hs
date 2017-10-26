module Pos.Arbitrary.Ssc
       ( SscPayloadDependsOnSlot (..)
       ) where

import           Universum

import           Test.QuickCheck (Gen)

import           Pos.Core        (SlotId)
import           Pos.Core.Ssc    (SscPayload)

newtype SscPayloadDependsOnSlot = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen SscPayload
    } deriving Generic
