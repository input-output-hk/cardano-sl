-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Pos.Binary.Block.Slog ()
import           Pos.Binary.Class      (Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Binary.Core       ()
import           Pos.Binary.Update     ()
import           Pos.Block.Slog.Types  (SlogUndo)
import           Pos.Block.Types       (Undo (..))
import           Pos.Core              (HasConfiguration)
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Txp.Core.Types    (TxpUndo)
import           Pos.Update.Poll.Types (USUndo)

deriveSimpleBiCxt [t|HasConfiguration|] ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo  |],
        Field [| undoDlg   :: DlgUndo  |],
        Field [| undoUS    :: USUndo   |],
        Field [| undoSlog  :: SlogUndo |]
    ]]
