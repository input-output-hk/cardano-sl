{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores stakes.

module Pos.Txp.DB.Balances
       (
         -- * Operations
         BalancesOp (..)

         -- * Initialization
       , prepareGStateBalances

                -- * Iteration
       , BalanceIter
       , runBalanceIterator
       , runBalanceMapIterator

         -- * Sanity checks
       , sanityCheckBalances
       ) where

import qualified Data.HashMap.Strict    as HM
import qualified Data.Text.Buildable
import qualified Database.RocksDB       as Rocks
import           Formatting             (bprint, bprint, sformat, (%))
import           Serokell.Util          (Color (Red), colorize)
import           System.Wlog            (WithLogger, logError)
import           Universum

import           Pos.Binary.Class       (encodeStrict)
import           Pos.Crypto             (shortHashF)
import           Pos.DB.Class           (MonadDB)
import           Pos.DB.Error           (DBError (..))
import           Pos.DB.Functions       (RocksBatchOp (..))
import           Pos.DB.GState.Balances (BalanceIter, ftsStakeKey, ftsSumKey, getFtsSumMaybe,
                                         getTotalFtsStake)
import           Pos.DB.GState.Common   (gsPutBi)
import           Pos.DB.Iterator        (DBnIterator, DBnMapIterator, IterType,
                                         runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types           (NodeDBs (_gStateDB))
import           Pos.Txp.Core           (txOutStake)
import           Pos.Txp.Toil.Types     (Utxo)
import           Pos.Txp.Toil.Utxo      (utxoToStakes)
import           Pos.Types              (Coin, StakeholderId, coinF, mkCoin, sumCoins,
                                         unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util.Iterator      (MonadIterator (..))

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
    :: forall m.
       MonadDB m
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

putTotalFtsStake :: MonadDB m => Coin -> m ()
putTotalFtsStake = gsPutBi ftsSumKey

----------------------------------------------------------------------------
-- Balance
----------------------------------------------------------------------------

runBalanceIterator
    :: forall m a . MonadDB m
    => DBnIterator BalanceIter a -> m a
runBalanceIterator = runDBnIterator @BalanceIter _gStateDB

runBalanceMapIterator
    :: forall v m a . MonadDB m
    => DBnMapIterator BalanceIter v a -> (IterType BalanceIter -> v) -> m a
runBalanceMapIterator = runDBnMapIterator @BalanceIter _gStateDB

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadMask m, MonadDB m, WithLogger m)
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
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB m => StakeholderId -> Coin -> m ()
putFtsStake = gsPutBi . ftsStakeKey
