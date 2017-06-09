{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic related to eligibility threshold.

module Pos.Lrc.Logic
       ( findRichmenStake
       , findRichmenPure
       , findAllRichmenMaybe
       , findDelegatedRichmen
       , RichmenType (..)
       ) where

import           Data.Conduit        (Sink, runConduit, runConduitPure, (.|))
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Universum

import           Pos.DB.Class        (MonadDBRead)
import           Pos.DB.GState       (getDelegators, getEffectiveStake,
                                      isIssuerByAddressHash)
import           Pos.Lrc.Core        (findDelegationStakes, findRichmenStake)
import           Pos.Lrc.Types       (FullRichmenData, RichmenStake)
import           Pos.Types           (Coin, StakeholderId, sumCoins, unsafeIntegerToCoin)

-- Can it be improved using conduits?
-- | Find delegated richmen using precomputed usual richmen.
-- Do it using one pass by delegation DB.
findDelRichUsingPrecomp
    :: forall m.
       (MonadDBRead m)
    => RichmenStake -> Coin -> m RichmenStake
findDelRichUsingPrecomp precomputed thr = do
    (old, new) <-
        runConduit $
        getDelegators .|
        findDelegationStakes isIssuerByAddressHash getEffectiveStake thr
    -- attention: order of new and precomputed is important
    -- we want to use new balances (computed from delegated) of precomputed richmen
    pure (new `HM.union` (precomputed `HM.difference` (HS.toMap old)))

-- | Find delegated richmen.
findDelegatedRichmen
    :: (MonadDBRead m)
    => Coin -> Sink (StakeholderId, Coin) m RichmenStake
findDelegatedRichmen thr = do
    st <- findRichmenStake thr
    lift $ findDelRichUsingPrecomp st thr

-- | Function considers all variants of computation
-- and compute using one pass by stake DB and one pass by delegation DB.
findAllRichmenMaybe
    :: forall m.
       (MonadDBRead m)
    => Maybe Coin -- ^ Eligibility threshold (optional)
    -> Maybe Coin -- ^ Delegation threshold (optional)
    -> Sink (StakeholderId, Coin) m (RichmenStake, RichmenStake)
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
    | RTDelegation (HashMap StakeholderId (HashSet StakeholderId))

-- | Pure version of findRichmen which uses in-memory Utxo.
findRichmenPure :: [(StakeholderId, Coin)]
                -> (Coin -> Coin)
                -> RichmenType
                -> FullRichmenData
findRichmenPure stakeDistribution thresholdF computeType
    | RTDelegation delegationMap <- computeType = do
        let issuers = mconcat $ HM.elems delegationMap
            (old, new) =
                runConduitPure $
                CL.sourceList (HM.toList delegationMap) .|
                (findDelegationStakes
                     (pure . flip HS.member issuers)
                     (pure . flip HM.lookup stakeMap) thresholdCoin)
        (total, new `HM.union` (usualRichmen `HM.difference` (HS.toMap old)))
    | otherwise = (total, usualRichmen)
  where
    stakeMap = HM.fromList stakeDistribution
    usualRichmen =
        runConduitPure $
        CL.sourceList stakeDistribution .| findRichmenStake thresholdCoin
    total = unsafeIntegerToCoin $ sumCoins $ map snd stakeDistribution
    thresholdCoin = thresholdF total
