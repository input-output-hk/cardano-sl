{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores stakeholders' balances.

module Pos.DB.GState.Balances
       (
         -- * Getters
         getTotalFtsStake
       , getFtsStake

         -- * Operations
       , BalancesOp (..)

         -- * Initialization
       , prepareGStateBalances

         -- * Iteration
       , iterateByStake

         -- * Sanity checks
       , sanityCheckBalances
       ) where

import qualified Data.HashMap.Strict  as HM
import qualified Database.RocksDB     as Rocks
import           Formatting           (sformat, (%))
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator    (DBMapIterator, mapIterator)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), WithKeyPrefix (..),
                                       encodeWithKeyPrefix)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.Types            (Coin, StakeholderId, Utxo, coinF, mkCoin, sumCoins,
                                       txOutStake, unsafeAddCoin, unsafeIntegerToCoin,
                                       utxoToStakes)
import           Pos.Util             (Color (Red), colorize, maybeThrow)
import           Pos.Util.Iterator    (MonadIterator (..))

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get total amount of stake to be used for follow-the-satoshi. It's
-- different from total amount of coins in the system.
getTotalFtsStake :: MonadDB ssc m => m Coin
getTotalFtsStake =
    maybeThrow (DBMalformed "no total FTS stake in GState DB") =<< getFtsSumMaybe

-- | Get stake owne by given stakeholder (according to rules used for FTS).
getFtsStake :: MonadDB ssc m => StakeholderId -> m (Maybe Coin)
getFtsStake = getBi . ftsStakeKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data BalancesOp
    = PutFtsSum !Coin
    | PutFtsStake !StakeholderId
                  !Coin

instance RocksBatchOp BalancesOp where
    toBatchOp (PutFtsSum c)      = [Rocks.Put ftsSumKey (encodeStrict c)]
    toBatchOp (PutFtsStake ad c) = [Rocks.Put (ftsStakeKey ad) (encodeStrict c)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBalances
    :: forall ssc m.
       MonadDB ssc m
    => Utxo -> m ()
prepareGStateBalances genesisUtxo = do
    putIfEmpty getFtsSumMaybe putFtsStakes
    putIfEmpty getFtsSumMaybe putGenesisTotalStake
  where
    totalCoins = sumCoins $ map snd $ concatMap txOutStake $ toList genesisUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    -- Will 'panic' if the result doesn't fit into Word64 (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) . HM.toList $ utxoToStakes genesisUtxo

putTotalFtsStake :: MonadDB ssc m => Coin -> m ()
putTotalFtsStake = putBi ftsSumKey

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

type IterType = (StakeholderId, Coin)

iterateByStake :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
                => DBMapIterator IterType v m a -> (IterType -> v) -> m a
iterateByStake iter f = mapIterator @IterType @v iter f =<< getUtxoDB

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadMask m, MonadDB ssc m, WithLogger m)
    => m ()
sanityCheckBalances = do
    let step sm = nextItem >>= maybe (pure sm) (\c -> step (unsafeAddCoin sm c))
    realTotalStake <- iterateByStake (step (mkCoin 0)) snd
    totalStake <- getTotalFtsStake
    let fmt =
            ("Wrong total FTS stake: \
             \real total FTS stake (sum of balances): "%coinF%
             ", but getTotalFtsStake returned: "%coinF)
    let msg = sformat fmt realTotalStake totalStake
    unless (realTotalStake == totalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

instance WithKeyPrefix StakeholderId where
    keyPrefix _ = "b/s"

ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB ssc m => StakeholderId -> Coin -> m ()
putFtsStake = putBi . ftsStakeKey

getFtsSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getFtsSumMaybe = getBi ftsSumKey
