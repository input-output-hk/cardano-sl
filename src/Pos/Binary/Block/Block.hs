-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class  (Bi (..), label, labelS, putField)
import           Pos.Binary.Core   ()
import           Pos.Binary.Update ()
import           Pos.Block.Types   (Undo (..))

instance Bi Undo where
    sizeNPut = labelS "Undo" $
        putField undoTx <>
        putField undoPsk <>
        putField undoUS
    get = label "Undo" $ Undo <$> get <*> get <*> get
