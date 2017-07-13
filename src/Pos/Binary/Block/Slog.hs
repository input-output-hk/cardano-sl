-- | Binary serialization of Slog types.

module Pos.Binary.Block.Slog
       (
       ) where

import           Universum

import qualified Pos.Binary.Cbor      as Cbor
import           Pos.Binary.Class     (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Block.Slog.Types (SlogUndo (..))
import           Pos.Core             (FlatSlotId)

deriveSimpleBi ''SlogUndo [
    Cons 'SlogUndo [
        Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]

Cbor.deriveSimpleBi ''SlogUndo [
    Cbor.Cons 'SlogUndo [
        Cbor.Field [| getSlogUndo  :: Maybe FlatSlotId |]
    ]]
