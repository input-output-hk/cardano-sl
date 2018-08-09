-- | You can treat this part as another block component which deals
-- with all data not related directly to any other component, while
-- another part of 'Pos.Block' is dedicated to whole blocks.

module Pos.Block.Slog
       ( module Pos.Block.Slog.Context
       , module Pos.Block.Slog.Logic
       , module Pos.Block.Slog.Types
       ) where

import           Pos.Block.Slog.Context
import           Pos.Block.Slog.Logic
import           Pos.Block.Slog.Types
