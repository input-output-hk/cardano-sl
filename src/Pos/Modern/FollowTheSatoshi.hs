{-# LANGUAGE MultiWayIf #-}

-- | Everything related to /follow-the-satoshi/ procedure.

module Pos.Modern.FollowTheSatoshi
       ( followTheSatoshi
       ) where




import           Data.List.NonEmpty (NonEmpty)
import           Universum

import           Pos.Modern.DB      (MonadDB)
import           Pos.Types.Types    (Address, SharedSeed)

followTheSatoshi :: MonadDB ssc m => SharedSeed -> m (NonEmpty Address)
followTheSatoshi _ = notImplemented
