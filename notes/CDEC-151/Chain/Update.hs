module Chain.Update where

import Block (Block, Point)

data ChainUpdate = AddBlock Block
                 | RollBack Point
  deriving Show
