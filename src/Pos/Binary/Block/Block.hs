-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Pos.Binary.Block.Slog ()
import           Pos.Binary.Class      (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core       ()
import           Pos.Binary.Update     ()
import           Pos.Block.Slog.Types  (SlogUndo)
import           Pos.Block.Types       (Undo (..))
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Txp.Core.Types    (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

deriveSimpleBi ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo   |],
        Field [| undoDlg   :: DlgUndo   |],
        Field [| undoUS    :: USUndo    |],
        Field [| undoSlog  :: SlogUndo  |]
    ]]
