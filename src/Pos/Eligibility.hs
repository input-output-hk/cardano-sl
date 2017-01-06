{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Eligibility
       ( findRichmen
       , findRichmenPure
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import           Universum

import           Pos.Types           (Coin, Richmen, StakeholderId, Utxo, txOutStake)
import           Pos.Util.Iterator   (MonadIterator (nextItem), runListHolder)

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenMap
    :: forall m.
       MonadIterator m (StakeholderId, Coin)
    => [Coin]                       -- ^ Eligibility threshold
    -> m ([HashMap StakeholderId Coin])
findRichmenMap thresholds = do
    hm <- execStateT step mempty
    pure $ map (flip HM.filter hm . (>=)) thresholds
  where
    threshold = minimum thresholds
    step :: StateT (HashMap StakeholderId Coin) m ()
    step = whenJustM nextItem $ tryAdd >=> \_ -> step
    tryAdd
        :: (StakeholderId, Coin)
        -> StateT (HashMap StakeholderId Coin) m ()
    -- Adding coins here should be safe because in utxo we're not supposed to
    -- ever have more coins than the total possible number of coins, and the
    -- total possible number of coins is less than Word64
    tryAdd (a, c) = when (c >= threshold) $ modify (HM.insert a c)

findRichmen
    :: forall m.
       MonadIterator m (StakeholderId, Coin)
    => Coin
    -> m Richmen
findRichmen threshold =
    fromMaybe onNoRichmen . NE.nonEmpty . HM.keys .
    fromMaybe (panic "Empty list") . head <$> findRichmenMap [threshold]
  where
    onNoRichmen = panic "There are no richmen!"

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: Utxo -> Coin -> Richmen
findRichmenPure utxo t =
    runListHolder (findRichmen t) . concatMap txOutStake $ toList utxo
