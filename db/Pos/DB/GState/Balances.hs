{-# LANGUAGE TypeFamilies #-}

-- |

module Pos.DB.GState.Balances
       ( BalIter
         -- * Getters
       , getTotalFtsStake
       , getFtsStake
       , getFtsSumMaybe

       , ftsStakeKey
       , ftsSumKey
       ) where

import           Universum

import           Pos.Binary.Core      ()
import           Pos.Core.Types       (Coin, StakeholderId)
import           Pos.DB.Class         (MonadDB)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (encodeWithKeyPrefix)
import           Pos.DB.Iterator      (DBIteratorClass (..))
import           Pos.Util.Util        (maybeThrow)

import           Pos.DB.GState.Common (gsGetBi)


data BalIter
instance DBIteratorClass BalIter where
    type IterKey BalIter = StakeholderId
    type IterValue BalIter = Coin
    iterKeyPrefix _ = iterationPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------
iterationPrefix :: ByteString
iterationPrefix = "b/s/"

ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix @BalIter

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get total amount of stake to be used for follow-the-satoshi. It's
-- different from total amount of coins in the system.
getTotalFtsStake :: MonadDB m => m Coin
getTotalFtsStake =
    maybeThrow (DBMalformed "no total FTS stake in GState DB") =<< getFtsSumMaybe

-- | Get stake owne by given stakeholder (according to rules used for FTS).
getFtsStake :: MonadDB m => StakeholderId -> m (Maybe Coin)
getFtsStake = gsGetBi . ftsStakeKey

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getFtsSumMaybe :: MonadDB m => m (Maybe Coin)
getFtsSumMaybe = gsGetBi ftsSumKey
