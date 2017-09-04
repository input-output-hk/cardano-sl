{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Part of GState DB which stores stakes.

module Pos.Txp.DB.Balances
       (
         -- * Operations
         BalancesOp (..)

         -- * Initialization
       , initGStateBalances

         -- * Iteration
       , BalanceIter
       , balanceSource
       , getAllPotentiallyHugeStakesMap

         -- * Sanity checks
       , sanityCheckBalances
       ) where

import           Universum

import           Control.Lens                 (at)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (Source, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List            as CL
import qualified Data.HashMap.Strict          as HM
import qualified Data.Text.Buildable
import qualified Database.RocksDB             as Rocks
import           Formatting                   (bprint, sformat, (%))
import           Serokell.Util                (Color (Red), colorize)
import           System.Wlog                  (WithLogger, logError)

import           Pos.Core                     (Coin, GenesisWStakeholders, StakeholderId,
                                               StakesMap, coinF, mkCoin, sumCoins,
                                               unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Crypto                   (shortHashF)
import           Pos.DB                       (DBError (..), DBTag (GStateDB), IterType,
                                               MonadDB, MonadDBRead, RocksBatchOp (..),
                                               dbIterSource, dbSerialize)
import           Pos.DB.GState.Balances       (BalanceIter, ftsStakeKey, ftsSumKey,
                                               getRealTotalStake)
import           Pos.DB.GState.Common         (gsPutBi)
import           Pos.Txp.Toil.Types           (GenesisUtxo (..))
import           Pos.Txp.Toil.Utxo            (utxoToStakes)

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
    toBatchOp (PutFtsSum c)      = [Rocks.Put ftsSumKey (dbSerialize c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkCoin 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (dbSerialize c)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateBalances
    :: forall m.
       MonadDB m
    => GenesisUtxo -> GenesisWStakeholders -> m ()
initGStateBalances (GenesisUtxo genesisUtxo) gws = do
    putFtsStakes
    putGenesisTotalStake
  where
    putTotalFtsStake = gsPutBi ftsSumKey
    genesisStakes = utxoToStakes gws genesisUtxo
    totalCoins = sumCoins genesisStakes
    -- Will 'error' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) . HM.toList $ genesisStakes

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Run iterator over effective balances.
balanceSource ::
       forall m. (MonadDBRead m)
    => Source (ResourceT m) (IterType BalanceIter)
balanceSource = dbIterSource GStateDB (Proxy @BalanceIter)

-- | Get stakes of all stakeholders. Use with care – the resulting map
-- can be very big.
getAllPotentiallyHugeStakesMap :: MonadDBRead m => m StakesMap
getAllPotentiallyHugeStakesMap =
    runConduitRes $
    dbIterSource GStateDB (Proxy @BalanceIter) .|
    CL.fold (\stakes (k, v) -> stakes & at k .~ Just v) mempty

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckBalances
    :: (MonadDBRead m, WithLogger m)
    => m ()
sanityCheckBalances = do
    calculatedTotalStake <- runConduitRes $
        mapOutput snd (dbIterSource GStateDB (Proxy @BalanceIter)) .|
        CL.fold unsafeAddCoin (mkCoin 0)

    totalStake <- getRealTotalStake
    let fmt = ("Wrong real total stake: \
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
