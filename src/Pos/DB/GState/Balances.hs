{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores stakeholders' balances.

module Pos.DB.GState.Balances
       (
         -- * Getters
         getTotalFtsStake
       , getFtsStake
       -- kostil for BalancesView
       , getFtsStakeFromDB

         -- * Operations
       , BalancesOp (..)

         -- * Initialization
       , prepareGStateBalances

         -- * Iteration
       , BalIter
       , runBalanceIterator
       , runBalanceMapIterator

         -- * Sanity checks
       , sanityCheckBalances
       ) where

import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.Buildable
import qualified Database.RocksDB     as Rocks
import           Formatting           (bprint, bprint, sformat, (%))
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Crypto           (shortHashF)
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), encodeWithKeyPrefix, rocksGetBi)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.DB.Iterator      (DBIterator, DBIteratorClass (..), DBMapIterator,
                                       IterType, mapIterator, runIterator)
import           Pos.DB.Types         (DB)
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

getFtsStakeFromDB :: (MonadIO m, MonadThrow m)
                  => StakeholderId
                  -> DB ssc
                  -> m (Maybe Coin)
getFtsStakeFromDB id = rocksGetBi (ftsStakeKey id)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data BalancesOp
    = PutFtsSum !Coin
    | PutFtsStake !StakeholderId
                  !Coin

instance Buildable BalancesOp where
    build (PutFtsSum c) = bprint ("PutFtsSum ("%coinF%")") c
    build (PutFtsStake ad c) =
        bprint ("PutFtsStake ("%shortHashF%", "%coinF%")") ad c

instance RocksBatchOp BalancesOp where
    toBatchOp (PutFtsSum c)      = [Rocks.Put ftsSumKey (encodeStrict c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkCoin 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (encodeStrict c)]

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

data BalIter

instance DBIteratorClass BalIter where
    type IterKey BalIter = StakeholderId
    type IterValue BalIter = Coin
    iterKeyPrefix _ = iterationPrefix

runBalanceMapIterator
    :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
    => DBMapIterator BalIter v m a -> (IterType BalIter -> v) -> m a
runBalanceMapIterator iter f = mapIterator @BalIter @v iter f =<< getUtxoDB

runBalanceIterator
    :: forall m ssc a . (MonadDB ssc m, MonadMask m)
    => DBIterator BalIter m a -> m a
runBalanceIterator iter = runIterator @BalIter iter =<< getUtxoDB
----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadMask m, MonadDB ssc m, WithLogger m)
    => m ()
sanityCheckBalances = do
    let step sm = nextItem >>= maybe (pure sm) (\c -> step (unsafeAddCoin sm c))
    realTotalStake <- runBalanceMapIterator (step (mkCoin 0)) snd
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
ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix @BalIter

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

iterationPrefix :: ByteString
iterationPrefix = "b/s"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB ssc m => StakeholderId -> Coin -> m ()
putFtsStake = putBi . ftsStakeKey

getFtsSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getFtsSumMaybe = getBi ftsSumKey
