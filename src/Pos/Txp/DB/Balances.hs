{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores stakes.

module Pos.Txp.DB.Balances
       (
         -- * Operations
         BalancesOp (..)

         -- * Getters
       , isBootstrapEra
       , getEffectiveTotalStake
       , getEffectiveStake

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
import qualified Pos.Constants          as Const
import           Pos.Crypto             (shortHashF)
import           Pos.DB.Class           (MonadDB)
import           Pos.DB.Error           (DBError (..))
import           Pos.DB.Functions       (RocksBatchOp (..))
import           Pos.DB.GState.Balances (BalanceIter, ftsStakeKey, ftsSumKey,
                                         getRealStakeSumMaybe)
import qualified Pos.DB.GState.Balances as GS
import           Pos.DB.GState.Common   (gsPutBi)
import           Pos.DB.Iterator        (IterType, runDBnIterator, runDBnMapIterator)
import           Pos.DB.Types           (NodeDBs (_gStateDB))
import           Pos.Genesis            (genesisBalances)
import           Pos.Txp.Core           (txOutStake)
import           Pos.Txp.Toil.Types     (Utxo)
import           Pos.Txp.Toil.Utxo      (utxoToStakes)
import           Pos.Types              (Coin, StakeholderId, coinF, mkCoin, sumCoins,
                                         unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util.Iterator      (MonadIterator (..), runListHolderT)

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
-- Overloaded getters (for fixed balances for bootstrap era)
----------------------------------------------------------------------------

-- TODO: provide actual implementation after corresponding
-- flag is actually stored in the DB
isBootstrapEra :: MonadDB m => m Bool
isBootstrapEra = pure $ not Const.isDevelopment && True

genesisFakeTotalStake :: Coin
genesisFakeTotalStake = unsafeIntegerToCoin $ sumCoins genesisBalances

getEffectiveTotalStake :: MonadDB m => m Coin
getEffectiveTotalStake = ifM isBootstrapEra
    (pure genesisFakeTotalStake)
    GS.getRealTotalStake

getEffectiveStake :: MonadDB m => StakeholderId -> m (Maybe Coin)
getEffectiveStake id = ifM isBootstrapEra
    (pure $ HM.lookup id genesisBalances)
    (GS.getRealStake id)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateBalances
    :: forall m.
       MonadDB m
    => Utxo -> m ()
prepareGStateBalances genesisUtxo = do
    putIfEmpty getRealStakeSumMaybe putFtsStakes
    putIfEmpty getRealStakeSumMaybe putGenesisTotalStake
  where
    totalCoins = sumCoins $ map snd $ concatMap txOutStake $ toList genesisUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    -- Will 'panic' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) . HM.toList $ utxoToStakes genesisUtxo

putTotalFtsStake :: MonadDB m => Coin -> m ()
putTotalFtsStake = gsPutBi ftsSumKey

----------------------------------------------------------------------------
-- Balance
----------------------------------------------------------------------------

-- | Run iterator over real balances.
runBalanceIterReal
    :: forall m a . MonadDB m
    => (forall iter . ( MonadIterator (IterType BalanceIter) iter
                      , MonadDB iter
                      ) => iter a)
    -> m a
runBalanceIterReal iter = runDBnIterator @BalanceIter _gStateDB iter

-- | Run iterator over effective balances.
runBalanceIterator
    :: forall m a . MonadDB m
    => (forall iter . ( MonadIterator (IterType BalanceIter) iter
                      , MonadDB iter
                      ) => iter a)
    -> m a
runBalanceIterator iter = ifM isBootstrapEra
    (flip runListHolderT (HM.toList genesisBalances) iter) $
    runBalanceIterReal iter

-- | Run map iterator over real balances.
runBalanceMapIterReal
    :: forall v m a . MonadDB m
    => (forall iter . ( MonadIterator v iter
                      , MonadDB iter
                      ) => iter a)
    -> ((StakeholderId, Coin) -> v)
    -> m a
runBalanceMapIterReal iter f = runDBnMapIterator @BalanceIter _gStateDB iter f

-- | Run map iterator over effective balances.
runBalanceMapIterator
    :: forall v m a . MonadDB m
    => (forall iter . ( MonadIterator v iter
                      , MonadDB iter
                      ) => iter a)
    -> ((StakeholderId, Coin) -> v)
    -> m a
runBalanceMapIterator iter f = ifM isBootstrapEra
    (flip runListHolderT (f <$> HM.toList genesisBalances) iter) $
    (runBalanceMapIterReal iter f)

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadMask m, MonadDB m, WithLogger m)
    => m ()
sanityCheckBalances = do
    let step sm = nextItem >>= maybe (pure sm) (\c -> step (unsafeAddCoin sm c))
    calculatedTotalStake <- runBalanceMapIterReal (step (mkCoin 0)) snd
    totalStake <- GS.getRealTotalStake
    let fmt =
            ("Wrong real total stake: \
             \sum of real stakes: "%coinF%
             ", but getRealTotalStake returned: "%coinF)
    let msg = sformat fmt calculatedTotalStake totalStake
    unless (calculatedTotalStake == totalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB m => StakeholderId -> Coin -> m ()
putFtsStake = gsPutBi . ftsStakeKey
