-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Universum

import           Pos.Binary.Class      (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core       ()
import           Pos.Binary.Update     ()
import           Pos.Block.Types       (SlogUndo (..), Undo (..))
import           Pos.Core              (FlatSlotId)
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Txp.Core.Types    (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

deriveSimpleBi ''SlogUndo [
    Cons 'SlogUndo [
        Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]

deriveSimpleBi ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo   |],
        Field [| undoPsk   :: DlgUndo   |],
        Field [| undoUS    :: USUndo    |],
        Field [| undoSlog  :: SlogUndo  |]
    ]]
