
-- | Logic related to eligibility threshold.

module Pos.Lrc.Logic
       ( findRichmenStakes
       , findRichmenPure
       , findAllRichmenMaybe
       , findDelegatedRichmen
       , RichmenType (..)
       ) where

import           Universum

import           Data.Conduit (Sink, runConduitPure, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Core (Coin, CoinPortion, StakeholderId, applyCoinPortionUp, sumCoins,
                           unsafeIntegerToCoin)
import           Pos.DB.Class (MonadDBRead, MonadGState)
import           Pos.DB.GState.Stakes (getRealStake)
import           Pos.Delegation (getDelegators, isIssuerByAddressHash)
import           Pos.Lrc.Core (findDelegationStakes, findRichmenStakes)
import           Pos.Lrc.Types (FullRichmenData, RichmenStakes)

type MonadDBReadFull m = (MonadDBRead m, MonadGState m)

-- Can it be improved using conduits?
-- | Find delegated richmen using precomputed usual richmen.
-- Do it using one pass by delegation DB.
findDelRichUsingPrecomp
    :: forall m.
       (MonadDBReadFull m)
    => RichmenStakes -> Coin -> m RichmenStakes
findDelRichUsingPrecomp precomputed thr = do
    (old, new) <-
        runConduitRes $
        getDelegators .|
        findDelegationStakes isIssuerByAddressHash getRealStake thr
    -- attention: order of new and precomputed is important
    -- we want to use new stakes (computed from delegated) of precomputed richmen
    pure (new `HM.union` (precomputed `HM.difference` (HS.toMap old)))

-- | Find delegated richmen.
findDelegatedRichmen
    :: (MonadDBReadFull m)
    => Coin -> Sink (StakeholderId, Coin) m RichmenStakes
findDelegatedRichmen thr = do
    st <- findRichmenStakes thr
    lift $ findDelRichUsingPrecomp st thr

-- | Function considers all variants of computation
-- and compute using one pass by stake DB and one pass by delegation DB.
findAllRichmenMaybe
    :: forall m.
       (MonadDBReadFull m)
    => Maybe Coin -- ^ Eligibility threshold (optional)
    -> Maybe Coin -- ^ Delegation threshold (optional)
    -> Sink (StakeholderId, Coin) m (RichmenStakes, RichmenStakes)
findAllRichmenMaybe maybeT maybeTD
    | Just t <- maybeT
    , Just tD <- maybeTD = do
        let mn = min t tD
        richmenMin <- findRichmenStakes mn
        let richmen = HM.filter (>= t) richmenMin
        let precomputedD = HM.filter (>= tD) richmenMin
        richmenD <- lift $ findDelRichUsingPrecomp precomputedD tD
        pure (richmen, richmenD)
    | Just t <- maybeT = (,mempty) <$> findRichmenStakes t
    | Just tD <- maybeTD = (mempty,) <$> findDelegatedRichmen tD
    | otherwise = pure (mempty, mempty)

data RichmenType
    = RTUsual
    -- | A map from delegates to issuers
    | RTDelegation (HashMap StakeholderId (HashSet StakeholderId))

-- | Pure version of 'findRichmen' which uses a list of stakeholders.
findRichmenPure :: [(StakeholderId, Coin)]
                -> CoinPortion    -- ^ Richman eligibility as % of total stake
                -> RichmenType
                -> FullRichmenData
findRichmenPure stakeDistribution threshold computeType
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
        CL.sourceList stakeDistribution .| findRichmenStakes thresholdCoin
    total = unsafeIntegerToCoin $ sumCoins $ map snd stakeDistribution
    thresholdCoin = applyCoinPortionUp threshold total
