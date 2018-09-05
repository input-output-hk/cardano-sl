{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Part of GState DB which stores stakes.

module Pos.DB.Txp.Stakes
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
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (Color (Red), colorize)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Txp (genesisStakes)
import           Pos.Core (Coin, StakeholderId, StakesMap, coinF, mkCoin,
                     sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Core.Genesis (GenesisData (..))
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (..), DBTag (GStateDB), IterType, MonadDB,
                     MonadDBRead, RocksBatchOp (..), dbIterSource)
import           Pos.DB.GState.Common (gsPutBi)
import           Pos.DB.GState.Stakes (StakeIter, ftsStakeKey, ftsSumKey,
                     getRealTotalStake)
import           Pos.Util.Wlog (WithLogger, logError)

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

instance RocksBatchOp StakesOp where
    toBatchOp (PutTotalStake c)  = [Rocks.Put ftsSumKey (serialize' c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkCoin 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (serialize' c)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateStakes :: MonadDB m => GenesisData -> m ()
initGStateStakes genesisData = do
    putFtsStakes
    putGenesisTotalStake
  where
    putTotalFtsStake = gsPutBi ftsSumKey
    stakes = genesisStakes genesisData
    totalCoins = sumCoins stakes
    -- Will 'error' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) $ HM.toList stakes

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Run iterator over stakes.
stakeSource ::
       forall m. (MonadDBRead m)
    => ConduitT () (IterType StakeIter) (ResourceT m) ()
stakeSource = dbIterSource GStateDB (Proxy @StakeIter)

-- | Get stakes of all stakeholders. Use with care – the resulting map
-- can be very big.
getAllPotentiallyHugeStakesMap ::
       (MonadDBRead m, MonadUnliftIO m) => m StakesMap
getAllPotentiallyHugeStakesMap =
    runConduitRes $
    stakeSource .|
    CL.fold (\stakes (k, v) -> stakes & at k .~ Just v) mempty

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckStakes
    :: (MonadDBRead m, MonadUnliftIO m, WithLogger m)
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
