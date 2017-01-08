{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Richmen.Eligibility
       ( findRichmenStake
       , findRichmenPure
       , findAllRichmenMaybe
       , findDelegatedRichmen
       ) where

import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import           Universum

import           Pos.DB.Class             (MonadDB)
import           Pos.DB.GState.Balances   (getFtsStake)
import           Pos.DB.GState.Delegation (isIssuerByAddressHash)
import           Pos.Types                (Coin, RichmenStake, StakeholderId, Utxo,
                                           mkCoin, txOutStake, unsafeAddCoin)
import           Pos.Util                 (getKeys)
import           Pos.Util.Iterator        (MonadIterator (nextItem), runListHolder,
                                           runListHolderT)

type SetRichmen = HashSet StakeholderId

findDelegationStakes
    :: forall ssc m . (MonadDB ssc m
                      , MonadIterator m (StakeholderId, [StakeholderId]))
    => Coin -> m (SetRichmen, RichmenStake) -- old richmen, new richmen
findDelegationStakes t = do
    (old, new) <- step mempty mempty
    pure (getKeys ((HS.toMap old) `HM.difference` new), new)
  where
    step :: SetRichmen
         -> RichmenStake
         -> m (SetRichmen, RichmenStake)
    step old new = nextItem @_ @(StakeholderId, [StakeholderId]) >>=
        maybe (pure (old, new))
        (\(delegate, issuers) -> do
            sumIssuers <-
              foldM (\cr id -> (unsafeAddCoin cr) <$> safeBalance id)
                    (mkCoin 0)
                    issuers
            isIss <- isIssuerByAddressHash delegate
            curStake <- if isIss then pure sumIssuers
                        else (unsafeAddCoin sumIssuers) <$> safeBalance delegate
            let newRichmen =
                  if curStake >= t then HM.insert delegate curStake new
                  else new

            oldRichmen <-
              foldM (\hs is ->
                       ifM ((>= t) <$> safeBalance is)
                           (pure $ HS.insert is hs) (pure hs))
                     old
                     issuers
            step oldRichmen newRichmen)
    safeBalance id = fromMaybe (mkCoin 0) <$> getFtsStake id

findDelRichUsingPrecomp
    :: MonadDB ssc m
    => RichmenStake -> Coin -> m RichmenStake
findDelRichUsingPrecomp precomputed t = do
    (old, new) <- runListHolderT @(StakeholderId, [StakeholderId])
                      (findDelegationStakes t) [] --fix it
    pure (precomputed `HM.difference` (HS.toMap old) `HM.union` new)

findDelegatedRichmen
    :: (MonadDB ssc m, MonadIterator m (StakeholderId, Coin))
    => Coin -> m RichmenStake
findDelegatedRichmen t =
    findRichmenStake t >>= flip findDelRichUsingPrecomp t

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenStake
    :: forall m . MonadIterator m (StakeholderId, Coin)
    => Coin                       -- ^ Eligibility threshold
    -> m RichmenStake
findRichmenStake t = step mempty
  where
    step :: RichmenStake -> m RichmenStake
    step hm = nextItem >>=
        maybe (pure hm)
              (\stake -> step (tryAdd stake hm))
    tryAdd
        :: (StakeholderId, Coin)
        -> HashMap StakeholderId Coin
        -> HashMap StakeholderId Coin
    -- Adding coins here should be safe because in utxo we're not supposed to
    -- ever have more coins than the total possible number of coins, and the
    -- total possible number of coins is less than Word64
    tryAdd (a, c) hm =
        if c >= t then HM.insert a c hm
        else hm

findAllRichmenMaybe
    :: forall ssc m . (MonadDB ssc m, MonadIterator m (StakeholderId, Coin))
    => Maybe Coin -- ^ Eligibility threshold (optional)
    -> Maybe Coin -- ^ Delegation threshold (optional)
    -> m (RichmenStake, RichmenStake)
findAllRichmenMaybe maybeT maybeTD
    | Just t <- maybeT
    , Just tD <- maybeTD = do
        let mn = min t tD
        richmenMin <- findRichmenStake mn
        let richmen = HM.filter (>= t) richmenMin
        let precomputedD = HM.filter (>= tD) richmenMin
        richmenD <- findDelRichUsingPrecomp precomputedD tD
        pure (richmen, richmenD)
    | Just t <- maybeT = (,mempty) <$> findRichmenStake t
    | Just tD <- maybeTD =
        (mempty,) <$> findDelegatedRichmen tD
    | otherwise = pure (mempty, mempty)

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: Utxo -> Coin -> RichmenStake
findRichmenPure utxo t =
    runListHolder (findRichmenStake t) .
                  concatMap txOutStake $ toList utxo
