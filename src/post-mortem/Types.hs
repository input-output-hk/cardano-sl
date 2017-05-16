module Types
    ( TxHash
    , BlockHash
    , Timestamp
    , Slot
    , NodeIndex
    ) where

import Universum

type TxHash = Text
type BlockHash = Text
type Timestamp = Integer
type Slot = (Word64, Word16)
type NodeIndex = Int
