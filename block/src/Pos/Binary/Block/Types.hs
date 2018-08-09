{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Types
       (
       ) where

import           Universum

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core ()
import           Pos.Binary.Delegation ()
import           Pos.Binary.Update ()
import           Pos.Block.Slog.Types (SlogUndo (..))
import           Pos.Block.Types (Undo (..))
import           Pos.Core (FlatSlotId, TxpUndo)
import           Pos.Delegation.Types (DlgUndo)
import           Pos.Update.Poll.Types (USUndo)

deriveSimpleBi ''SlogUndo [
    Cons 'SlogUndo [
        Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]

deriveSimpleBi ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo  |],
        Field [| undoDlg   :: DlgUndo  |],
        Field [| undoUS    :: USUndo   |],
        Field [| undoSlog  :: SlogUndo |]
    ]]
