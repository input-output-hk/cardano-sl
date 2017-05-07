{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Lrc.Logic
       ( findRichmenStake
       , findRichmenPure
       , findAllRichmenMaybe
       , findDelegatedRichmen
       , RichmenType (..)
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Universum

import           Pos.Crypto.Signing  (pskDelegatePk)
import           Pos.DB.Class        (MonadDB)
import           Pos.DB.GState       (getEffectiveStake, isIssuerByAddressHash,
                                      runPskMapIterator)
import           Pos.Lrc.Core        (findDelegationStakes, findRichmenStake)
import           Pos.Lrc.Types       (FullRichmenData, RichmenStake)
import           Pos.Types           (Coin, StakeholderId, addressHash, sumCoins,
                                      unsafeIntegerToCoin)
import           Pos.Util.Iterator   (MonadIterator (nextItem), runListHolder,
                                      runListHolderT)

-- | Find delegated richmen using precomputed usual richmen.
-- Do it using one pass by delegation DB.
findDelRichUsingPrecomp
    :: forall m.
       MonadDB m
    => RichmenStake -> Coin -> m RichmenStake
findDelRichUsingPrecomp precomputed t = do
    delIssMap <- computeDelIssMap
    (old, new) <- runListHolderT @(StakeholderId, [StakeholderId])
                      (findDelegationStakes isIssuerByAddressHash getEffectiveStake t)
                      (HM.toList delIssMap)
    -- attention: order of new and precomputed is important
    -- we want to use new balances (computed from delegated) of precomputed richmen
    pure (new `HM.union` (precomputed `HM.difference` (HS.toMap old)))
  where
    computeDelIssMap :: m (HashMap StakeholderId [StakeholderId])
    computeDelIssMap =
        runPskMapIterator (step mempty) conv
    step hm = nextItem >>= maybe (pure hm) (\(iss, del) -> do
        let curList = HM.lookupDefault [] del hm
        step (HM.insert del (iss:curList) hm))
    conv (id, cert) = (id, addressHash (pskDelegatePk cert))

-- | Find delegated richmen.
findDelegatedRichmen
    :: (MonadDB m, MonadIterator (StakeholderId, Coin) m)
    => Coin -> m RichmenStake
findDelegatedRichmen t =
    findRichmenStake t >>= flip findDelRichUsingPrecomp t

-- | Function considers all variants of computation
-- and compute using one pass by stake DB and one pass by delegation DB.
findAllRichmenMaybe
    :: forall m.
       (MonadDB m, MonadIterator (StakeholderId, Coin) m)
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
