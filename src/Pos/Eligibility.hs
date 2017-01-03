{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Eligibility
       ( findRichmen
       , findRichmenPure
       ) where

import           Control.Lens        (at, (%=))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import           Universum

import           Pos.Types           (Coin, Richmen, StakeholderId, TxOutAux, Utxo,
                                      txOutStake)
import           Pos.Types.Coin      (unsafeAddCoin)
import           Pos.Util.Iterator   (MonadIterator (nextItem), runListHolder)

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmen
    :: forall m.
       MonadIterator m TxOutAux
    => Coin                       -- ^ Eligibility threshold
    -> m Richmen
findRichmen moneyT =
    fromMaybe onNoRichmen . NE.nonEmpty . HM.keys . HM.filter (>= moneyT) <$>
    execStateT step mempty
  where
    onNoRichmen = panic "There are no richmen!"
    step :: StateT (HashMap StakeholderId Coin) m ()
    step = whenJustM nextItem $ \txo -> for_ (txOutStake txo) innerStep
    innerStep
        :: (StakeholderId, Coin)
        -> StateT (HashMap StakeholderId Coin) m ()
    -- Adding coins here should be safe because in utxo we're not supposed to
    -- ever have more coins than the total possible number of coins, and the
    -- total possible number of coins is less than Word64
    innerStep (a, c) = at a %= (Just . maybe c (unsafeAddCoin c))

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: Utxo -> Coin -> Richmen
findRichmenPure utxo t = runListHolder (findRichmen t) $ toList utxo
