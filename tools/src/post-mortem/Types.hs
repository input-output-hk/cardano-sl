module Types
    ( TxHash
    , BlockHash
    , Timestamp
    , Slot
    , NodeId
    ) where

import           Data.Time.Units (Microsecond)
import           Universum

type TxHash = Text
type BlockHash = Text
type Timestamp = Microsecond
type Slot = (Word64, Word16)
type NodeId = Text
