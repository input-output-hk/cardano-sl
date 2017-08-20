-- | Binary serialization of Slog types.

module Pos.Binary.Block.Slog
       (
       ) where

import           Universum

import           Pos.Binary.Class     (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Block.Slog.Types (SlogUndo (..))
import           Pos.Core             (FlatSlotId)

deriveSimpleBi ''SlogUndo [
    Cons 'SlogUndo [
        Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]
