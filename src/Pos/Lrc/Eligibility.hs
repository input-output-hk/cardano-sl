{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Lrc.Eligibility
       ( findRichmenStake
       , findRichmenPure
       , findAllRichmenMaybe
       , findDelegatedRichmen
       , RichmenType (..)
       ) where

import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import           Universum

import           Pos.Crypto.Signing       (pskDelegatePk)
import           Pos.DB.Class             (MonadDB)
import           Pos.DB.GState.Balances   (getFtsStake)
import           Pos.DB.GState.Delegation (IssuerPublicKey (..), isIssuerByAddressHash,
                                           iteratePSKs)
import           Pos.Lrc.Types            (FullRichmenData, RichmenStake)
import           Pos.Types                (Coin, StakeholderId, addressHash, mkCoin,
                                           sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Util                 (getKeys)
import           Pos.Util.Iterator        (MonadIterator (nextItem), runListHolder,
                                           runListHolderT)

type SetRichmen = HashSet StakeholderId

-- | Function helper for delegated richmen.
-- Iterate by Delegate -> [Issuer] map and compute two set:
-- 1. Old richmen who delegated own stake and isn't richman more.
-- 2. Delegates who became richmen.
findDelegationStakes
    :: forall m . MonadIterator m (StakeholderId, [StakeholderId])
    => (StakeholderId -> m Bool) -- helper
    -> (StakeholderId -> m (Maybe Coin)) -- helper
    -> Coin
    -> m (SetRichmen, RichmenStake) -- old richmen, new richmen
findDelegationStakes isIssuer stakeResolver t = do
    (old, new) <- step (mempty, mempty)
    pure (getKeys ((HS.toMap old) `HM.difference` new), new)
  where
    step :: (SetRichmen, RichmenStake)
         -> m (SetRichmen, RichmenStake)
    step richmen = nextItem @_ @(StakeholderId, [StakeholderId]) >>=
        maybe (pure richmen) (onItem richmen >=> step)
    onItem (old, new) (delegate, issuers) = do
        sumIssuers <-
          foldM (\cr id -> (unsafeAddCoin cr) <$> safeBalance id)
                (mkCoin 0)
                issuers
        isIss <- isIssuer delegate
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
        pure (oldRichmen, newRichmen)
    safeBalance id = fromMaybe (mkCoin 0) <$> stakeResolver id

-- | Find delegated richmen using precomputed usual richmen.
-- Do it using one pass by delegation DB.
findDelRichUsingPrecomp
    :: forall ssc m . (MonadDB ssc m, MonadMask m)
    => RichmenStake -> Coin -> m RichmenStake
findDelRichUsingPrecomp precomputed t = do
    delIssMap <- computeDelIssMap
    (old, new) <- runListHolderT @(StakeholderId, [StakeholderId])
                      (findDelegationStakes isIssuerByAddressHash getFtsStake t)
                      (HM.toList delIssMap)
    -- attention: order of new and precomputed is important
    -- we want to use new balances (computed from delegated) of precomputed richmen
    pure (new `HM.union` (precomputed `HM.difference` (HS.toMap old)))
  where
    computeDelIssMap :: m (HashMap StakeholderId [StakeholderId])
    computeDelIssMap =
        iteratePSKs @(StakeholderId, StakeholderId) (step mempty) conv
    step hm = nextItem >>= maybe (pure hm) (\(iss, del) -> do
        let curList = HM.lookupDefault [] del hm
        step (HM.insert del (iss:curList) hm))
    conv (IssuerPublicKey id, cert) = (id, addressHash (pskDelegatePk cert))

-- | Find delegated richmen.
findDelegatedRichmen
    :: (MonadDB ssc m, MonadMask m, MonadIterator m (StakeholderId, Coin))
    => Coin -> m RichmenStake
findDelegatedRichmen t =
    findRichmenStake t >>= flip findDelRichUsingPrecomp t

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenStake
    :: forall m . MonadIterator m (StakeholderId, Coin)
    => Coin  -- ^ Eligibility threshold
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

-- | Function considers all variants of computation
-- and compute using one pass by stake DB and one pass by delegation DB.
findAllRichmenMaybe
    :: forall ssc m . (MonadDB ssc m, MonadMask m
                      , MonadIterator m (StakeholderId, Coin))
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
    | Just tD <- maybeTD = (mempty,) <$> findDelegatedRichmen tD
    | otherwise = pure (mempty, mempty)

data RichmenType
    = RTUsual
    | RTDelegation (HashMap StakeholderId [StakeholderId])

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: [(StakeholderId, Coin)]
                -> (Coin -> Coin)
                -> RichmenType
                -> FullRichmenData
findRichmenPure stakeDistribution thresholdF computeType
    | RTDelegation delegationMap <- computeType =
        let issuers = HS.fromList (concat $ toList delegationMap)
            (old, new) =
                runListHolder
                    (findDelegationStakes
                        (pure . flip HS.member issuers)
                        (pure . flip HM.lookup stakeMap) thresholdCoin)
                    (HM.toList delegationMap) in
        (total, new `HM.union` (usualRichmen `HM.difference` (HS.toMap old)))
    | otherwise = (total, usualRichmen)
  where
    stakeMap = HM.fromList stakeDistribution
    usualRichmen = runListHolder (findRichmenStake thresholdCoin) stakeDistribution
    total = unsafeIntegerToCoin $ sumCoins $ map snd stakeDistribution
    thresholdCoin = thresholdF total
