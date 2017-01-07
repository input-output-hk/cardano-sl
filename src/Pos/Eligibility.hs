{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Eligibility
       ( findRichmenStake
       , findRichmenPure
       ) where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Types           (Coin, RichmenStake, StakeholderId, Utxo, txOutStake)
import           Pos.Util.Iterator   (MonadIterator (nextItem), runListHolder)

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenStake
    :: forall m.
       MonadIterator m (StakeholderId, Coin)
    => Maybe Coin                       -- ^ Eligibility threshold (optional)
    -> Maybe Coin                       -- ^ Delegation threshold (optional)
    -> m (RichmenStake, RichmenStake)
findRichmenStake threshold dThreshold = step mempty mempty
  where
    step :: RichmenStake
         -> RichmenStake
         -> m (RichmenStake, RichmenStake)
    step hm dHm = nextItem >>=
        maybe (pure (hm, dHm))
              (\stake -> step (tryAdd threshold stake hm) (tryAdd dThreshold stake dHm))
    tryAdd
        :: Maybe Coin
        -> (StakeholderId, Coin)
        -> HashMap StakeholderId Coin
        -> HashMap StakeholderId Coin
    -- Adding coins here should be safe because in utxo we're not supposed to
    -- ever have more coins than the total possible number of coins, and the
    -- total possible number of coins is less than Word64
    tryAdd t (a, c) hm =
        if maybe False (c >= ) t then HM.insert a c hm
        else hm

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: Utxo -> Coin -> RichmenStake
findRichmenPure utxo t =
    runListHolder (fst <$> findRichmenStake (Just t) Nothing) . concatMap txOutStake $ toList utxo
  -- where
  --   findRichmen =
  --     fromMaybe onNoRichmen .
  --     NE.nonEmpty .
  --     HM.keys .
  --     fst
  --     <$> findRichmenStake (Just t) Nothing
  --   onNoRichmen = panic "There are no richmen!"
