-- | Binary serialization of Pos.Block.* Types.

module Pos.Binary.Block.Block
       (
       ) where

import           Universum

import           Pos.Binary.Block.Slog ()
import           Pos.Binary.Class      (Bi (..), encodeListLen, enforceSize)
import           Pos.Binary.Core       ()
import           Pos.Binary.Update     ()
import           Pos.Block.Types       (Undo (..))
import           Pos.Core.Context      (HasCoreConstants)

instance HasCoreConstants => Bi Undo where
    encode Undo{..} = encodeListLen 4 <>
        encode undoTx <>
        encode undoDlg <>
        encode undoUS <>
        encode undoSlog
    decode = do
        enforceSize "Undo" 4
        Undo <$>
            decode <*>
            decode <*>
            decode <*>
            decode
