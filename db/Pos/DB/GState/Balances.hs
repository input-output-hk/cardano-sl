{-# LANGUAGE TypeFamilies #-}

-- | Base database Balances operations.

module Pos.DB.GState.Balances
       ( BalanceIter
         -- * Getters
       , getRealTotalStake
       , getRealStake
       , getRealStakeSumMaybe

       , ftsStakeKey
       , ftsSumKey
       ) where

import           Universum

import           Pos.Binary.Core      ()
import           Pos.Core.Types       (Coin, StakeholderId)
import           Pos.Util.Util        (maybeThrow)

import           Pos.DB.Class         (MonadDBPure)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.DB.Iterator      (DBIteratorClass (..))

data BalanceIter

instance DBIteratorClass BalanceIter where
    type IterKey BalanceIter = StakeholderId
    type IterValue BalanceIter = Coin
    iterKeyPrefix _ = iterationPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

iterationPrefix :: ByteString
iterationPrefix = "b/s/"

ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix @BalanceIter

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get real total amount of stake to be used for follow-the-satoshi
-- and other procedures which use stake of some stakeholder.
--
-- Word `real` is essential here, because there is also apparent stake
-- during bootstrap era.
--
-- It's different from total amount of spendable coins in the system,
-- because spending is done using Utxo. For example, one can send
-- coins to script address and not include it into transaction
-- distribution.
getRealTotalStake :: MonadDBPure m => m Coin
getRealTotalStake =
    maybeThrow (DBMalformed "no total FTS stake in GState DB") =<< getRealStakeSumMaybe

-- | Get real stake owned by given stakeholder (according to rules
-- used for FTS and similar procedures). Word `real` is essential
-- here. See documentation of 'getRealTotalStake' for some
-- explanation.
getRealStake :: MonadDBPure m => StakeholderId -> m (Maybe Coin)
getRealStake = gsGetBi . ftsStakeKey

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getRealStakeSumMaybe :: MonadDBPure m => m (Maybe Coin)
getRealStakeSumMaybe = gsGetBi ftsSumKey
