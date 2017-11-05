-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Undo
       (
       ) where

import           Pos.Binary.Class      (Cons (..), Field (..), deriveSimpleBiCxt)
import           Pos.Binary.Core       ()
import           Pos.Binary.Delegation ()
import           Pos.Binary.Slog       ()
import           Pos.Binary.Update     ()
import           Pos.Block.Slog.Types  (SlogUndo)
import           Pos.Block.Types       (Undo (..))
import           Pos.Core              (HasConfiguration, TxpUndo)
import           Pos.Delegation.Types  (DlgUndo)
import           Pos.Update.Poll.Types (USUndo)

deriveSimpleBiCxt [t|HasConfiguration|] ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo  |],
        Field [| undoDlg   :: DlgUndo  |],
        Field [| undoUS    :: USUndo   |],
        Field [| undoSlog  :: SlogUndo |]
    ]]
