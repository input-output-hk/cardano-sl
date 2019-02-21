module Pos.DB.GState.ChainDifficulty
       (
         -- * Getters
         getMaxSeenDifficulty
       , getMaxSeenDifficultyMaybe

         -- * Keys
       , maxSeenDifficultyKey

         -- * Details
       , putMaxSeenDifficulty
       ) where

import           Universum

import           Pos.Core (ChainDifficulty)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get maximum seen chain difficulty (used to prevent improper rollbacks).
getMaxSeenDifficulty :: MonadDBRead m => m ChainDifficulty
getMaxSeenDifficulty =
    maybeThrow (DBMalformed "no max chain difficulty in GState DB") =<<
    getMaxSeenDifficultyMaybe

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

maxSeenDifficultyKey :: ByteString
maxSeenDifficultyKey = "c/maxsd"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getMaxSeenDifficultyMaybe :: MonadDBRead m => m (Maybe ChainDifficulty)
getMaxSeenDifficultyMaybe = gsGetBi maxSeenDifficultyKey

putMaxSeenDifficulty :: MonadDB m => ChainDifficulty -> m ()
putMaxSeenDifficulty = gsPutBi maxSeenDifficultyKey
