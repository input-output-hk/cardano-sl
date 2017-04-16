{-# LANGUAGE DeriveAnyClass #-}

-- | Block Auxiliary to store block in file.

module Pos.DB.Block.Aux
       ( BlundLocation (..)
       ) where

import           Data.Binary (Binary)
import           Universum

-- | Structure describes location of block and undo with specific header hash.
-- Block and corresponding Undo are stored in same file.
data BlundLocation = BlundLocation
    { blundFileNum :: !Word32 -- ^ In which file the blund is stored.
    , blockOffset  :: !Word32 -- ^ Offset of blob of the block in a file.
    , blockLen     :: !Word32 -- ^ Length of blob of the block.
    , undoOffset   :: !Word32 -- ^ Offset of blob of the undo in a file.
    , undoLen      :: !Word32 -- ^ Length of blob of the undo.
    } deriving (Show, Binary, Generic)
