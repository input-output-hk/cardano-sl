{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Part of GState DB which stores stakes.

module Pos.Txp.DB.Stakes
       (
         -- * Operations
         StakesOp (..)

         -- * Initialization
       , initGStateStakes

         -- * Iteration
       , StakeIter
       , stakeSource
       , getAllPotentiallyHugeStakesMap

         -- * Sanity checks
       , sanityCheckStakes
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (Source, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, sformat, (%))
import           Serokell.Util (Color (Red), colorize)
import           System.Wlog (WithLogger, logError)

import           Pos.Core (Coin, HasConfiguration, StakeholderId, StakesMap, coinF, mkCoin,
                           sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (..), DBTag (GStateDB), IterType, MonadDB, MonadDBRead,
                         RocksBatchOp (..), dbIterSource, dbSerializeValue)
import           Pos.DB.GState.Common (gsPutBi)
import           Pos.DB.GState.Stakes (StakeIter, ftsStakeKey, ftsSumKey, getRealTotalStake)
import           Pos.Txp.Toil.Types (GenesisUtxo (..))
import           Pos.Txp.Toil.Utxo (utxoToStakes)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data StakesOp
    = PutTotalStake !Coin
    | PutFtsStake !StakeholderId
                  !Coin

instance Buildable StakesOp where
    build (PutTotalStake c) = bprint ("PutTotalStake ("%coinF%")") c
    build (PutFtsStake ad c) =
        bprint ("PutFtsStake ("%shortHashF%", "%coinF%")") ad c

instance HasConfiguration => RocksBatchOp StakesOp where
    toBatchOp (PutTotalStake c)  = [Rocks.Put ftsSumKey (dbSerializeValue c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkCoin 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (dbSerializeValue c)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateStakes ::
       forall m. MonadDB m
    => GenesisUtxo
    -> m ()
initGStateStakes (GenesisUtxo genesisUtxo) = do
    putFtsStakes
    putGenesisTotalStake
  where
    putTotalFtsStake = gsPutBi ftsSumKey
    genesisStakes = utxoToStakes genesisUtxo
    totalCoins = sumCoins genesisStakes
    -- Will 'error' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) . HM.toList $ genesisStakes

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Run iterator over stakes.
stakeSource ::
       forall m. (MonadDBRead m)
    => Source (ResourceT m) (IterType StakeIter)
stakeSource = dbIterSource GStateDB (Proxy @StakeIter)

-- | Get stakes of all stakeholders. Use with care – the resulting map
-- can be very big.
getAllPotentiallyHugeStakesMap :: MonadDBRead m => m StakesMap
getAllPotentiallyHugeStakesMap =
    runConduitRes $
    stakeSource .|
    CL.fold (\stakes (k, v) -> stakes & at k .~ Just v) mempty

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckStakes
    :: (MonadDBRead m, WithLogger m)
    => m ()
sanityCheckStakes = do
    calculatedTotalStake <- runConduitRes $
        mapOutput snd stakeSource .|
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
