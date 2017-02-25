-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Data.Binary.Get   (label)
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Binary.Core  ()
import           Pos.Binary.Update ()
import           Pos.Block.Types   (Undo (..))

instance Bi Undo where
    put (Undo txs psks usUndo) = put txs >> put psks >> put usUndo
    get = label "Undo" $ Undo <$> get <*> get <*> get
