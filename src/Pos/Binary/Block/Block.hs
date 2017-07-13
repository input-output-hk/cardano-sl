-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Pos.Binary.Block.Slog ()
import           Pos.Binary.Class      (Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Binary.Cbor       as Cbor
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
        Field [| undoPsk   :: DlgUndo   |],
        Field [| undoUS    :: USUndo    |],
        Field [| undoSlog  :: SlogUndo  |]
    ]]

Cbor.deriveSimpleBi ''Undo [
    Cbor.Cons 'Undo [
        Cbor.Field [| undoTx    :: TxpUndo  |],
        Cbor.Field [| undoPsk   :: DlgUndo  |],
        Cbor.Field [| undoUS    :: USUndo   |],
        Cbor.Field [| undoSlog  :: SlogUndo |]
    ]]
