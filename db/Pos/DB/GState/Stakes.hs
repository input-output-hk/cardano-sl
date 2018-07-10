{-# LANGUAGE TypeFamilies #-}

-- | Base database Stakes operations.

module Pos.DB.GState.Stakes
       ( StakeIter
         -- * Getters
       , getRealTotalStake
       , getRealStake
       , getRealStakeSumMaybe

       , ftsStakeKey
       , ftsSumKey
       ) where

import           Universum

import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Configuration (CoreConfiguration)
import           Pos.DB.Class (DBIteratorClass (..), MonadDBRead)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.Util.Util (maybeThrow)


data StakeIter

instance DBIteratorClass StakeIter where
    type IterKey StakeIter = StakeholderId
    type IterValue StakeIter = Coin
    iterKeyPrefix = iterationPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

iterationPrefix :: ByteString
iterationPrefix = "b/s/"

ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix @StakeIter

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get real total amount of stake to be used for follow-the-satoshi
-- and other procedures which use stake of some stakeholder.
getRealTotalStake :: MonadDBRead m => CoreConfiguration -> m Coin
getRealTotalStake cc =
    getRealStakeSumMaybe cc
       >>= maybeThrow (DBMalformed "no total FTS stake in GState DB")

-- | Get real stake owned by given stakeholder (according to rules
-- used for FTS and similar procedures).
getRealStake :: MonadDBRead m => CoreConfiguration -> StakeholderId -> m (Maybe Coin)
getRealStake cc sId = gsGetBi cc (ftsStakeKey sId)

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getRealStakeSumMaybe :: MonadDBRead m => CoreConfiguration -> m (Maybe Coin)
getRealStakeSumMaybe cc = gsGetBi cc ftsSumKey
