module Pos.Lrc.Genesis
       ( genesisSeed
       ) where

import           Pos.Core.Types (SharedSeed (..))

-- | Seed that will be used for the 0th epoch. We must hardcode a seed
-- because we need to somehow determine leaders for the first ever epoch
-- (stakes are hardcoded as well so we can run FTS on those stakes using this
-- seed).
genesisSeed :: SharedSeed
genesisSeed = SharedSeed "vasa opasa skovoroda Ggurda boroda provoda"
