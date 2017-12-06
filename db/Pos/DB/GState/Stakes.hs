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

import           Pos.Binary.Core ()
import           Pos.Core.Common (Coin, StakeholderId)
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
getRealTotalStake :: MonadDBRead m => m Coin
getRealTotalStake =
    maybeThrow (DBMalformed "no total FTS stake in GState DB") =<< getRealStakeSumMaybe

-- | Get real stake owned by given stakeholder (according to rules
-- used for FTS and similar procedures).
getRealStake :: MonadDBRead m => StakeholderId -> m (Maybe Coin)
getRealStake = gsGetBi . ftsStakeKey

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getRealStakeSumMaybe :: MonadDBRead m => m (Maybe Coin)
getRealStakeSumMaybe = gsGetBi ftsSumKey
