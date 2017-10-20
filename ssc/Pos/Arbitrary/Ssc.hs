module Pos.Arbitrary.Ssc
       ( SscPayloadDependsOnSlot (..)
       ) where

import           Universum

import           Test.QuickCheck     (Gen)

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Core            (SlotId)

newtype SscPayloadDependsOnSlot = SscPayloadDependsOnSlot
    { genPayloadDependsOnSlot :: SlotId -> Gen SscPayload
    } deriving Generic
