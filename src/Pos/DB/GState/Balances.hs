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
       ) where

import qualified Data.Map             as M
import qualified Database.RocksDB     as Rocks
import           Mockable             (Bracket, Catch, Mockable, Throw)
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (RocksBatchOp (..), traverseAllEntries)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.Types            (Coin, StakeholderId, Utxo, sumCoins, txOutStake,
                                       unsafeIntegerToCoin)
import           Pos.Util             (maybeThrow')

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get total amount of stake to be used for follow-the-satoshi. It's
-- different from total amount of coins in the system.
getTotalFtsStake :: MonadDB ssc m => m Coin
getTotalFtsStake =
    maybeThrow' (DBMalformed "no total FTS stake in GState DB") =<< getFtsSumMaybe

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
    putIfEmpty getFtsSumMaybe putGenesisSum
  where
    totalCoins = sumCoins $ map snd $ concatMap txOutStake $ toList genesisUtxo
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter
    -- Will 'panic' if the result doesn't fit into Word64 (which should never
    -- happen)
    putGenesisSum = putTotalFtsStake (unsafeIntegerToCoin totalCoins)
    putFtsStakes = mapM_ putFtsStake' $ M.toList genesisUtxo
    putFtsStake' (_, toaux) = mapM (uncurry putFtsStake) (txOutStake toaux)

putTotalFtsStake :: MonadDB ssc m => Coin -> m ()
putTotalFtsStake = putBi ftsSumKey

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

iterateByStake
    :: forall ssc m . (MonadDB ssc m, Mockable Catch m, Mockable Bracket m, Mockable Throw m)
    => ((StakeholderId, Coin) -> m ())
    -> m ()
iterateByStake callback = do
    db <- getUtxoDB
    traverseAllEntries db (pure ()) $ const $ curry callback

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

ftsStakeKey :: StakeholderId -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- ftsStakeKey = (<> "b/s") . encodeStrict
ftsStakeKey = encodeStrict

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB ssc m => StakeholderId -> Coin -> m ()
putFtsStake = putBi . ftsStakeKey

getFtsSumMaybe :: (MonadDB ssc m) => m (Maybe Coin)
getFtsSumMaybe = getBi ftsSumKey
