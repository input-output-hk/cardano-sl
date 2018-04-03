
-- | Core logic of LRC.

module Pos.Lrc.Core
       ( findDelegationStakes
       , findRichmenStakes
       ) where

import           Universum hiding (id)

import           Data.Conduit (ConduitT, await)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Core.Common (Coin, StakeholderId, mkCoin, unsafeAddCoin)
import           Pos.Lrc.Types (RichmenSet, RichmenStakes)
import           Pos.Util.Util (getKeys)


-- | Function helper for delegated richmen. Iterates @Delegate ->
-- [Issuer]@ map and computes the following two sets:
--
-- 1. Old richmen set: those who delegated their own stake and thus
-- lost richmen status.
--
-- 2. Delegates who became richmen.
findDelegationStakes
    :: forall m . Monad m
    => (StakeholderId -> m Bool)                   -- ^ Check if user is issuer?
    -> (StakeholderId -> m (Maybe Coin))           -- ^ Gets effective stake.
    -> Coin                                        -- ^ Coin threshold
    -> ConduitT (StakeholderId, HashSet StakeholderId)
                Void
                m
                (RichmenSet, RichmenStakes)            -- ^ Old richmen, new richmen
findDelegationStakes isIssuer stakeResolver t = do
    (old, new) <- step (mempty, mempty)
    pure (getKeys ((HS.toMap old) `HM.difference` new), new)
  where
    step :: (RichmenSet, RichmenStakes)
         -> ConduitT (StakeholderId, HashSet StakeholderId)
                     Void
                     m
                     (RichmenSet, RichmenStakes)
    step richmen = do
        v <- await
        maybe (pure richmen) (onItem richmen >=> step) v
    onItem (old, new) (delegate, issuers) = do
        sumIssuers <-
            foldM (\cr id -> (unsafeAddCoin cr) <$> safeGetStake id)
                  (mkCoin 0)
                  issuers
        isIss <- lift $ isIssuer delegate
        curStake <- if isIss then pure sumIssuers
                    else (unsafeAddCoin sumIssuers) <$> safeGetStake delegate
        let newRichmen =
              if curStake >= t then HM.insert delegate curStake new
              else new

        oldRichmen <-
            foldM (\hs is -> ifM ((>= t) <$> safeGetStake is)
                                 (pure $ HS.insert is hs)
                                 (pure hs))
                  old
                  issuers
        pure (oldRichmen, newRichmen)
    safeGetStake id = fromMaybe (mkCoin 0) <$> lift (stakeResolver id)

-- | Find all stake holders which have at least 'eligibility threshold' coins.
-- Assumes that the `StakeholderId`s are unique. The consumer of the  generated
-- `RichmenStakes` further assumes that the provided `Coin` values are valid,
-- and that the sum of the input `Coin` values is less than `maxCoinVal`.
findRichmenStakes
    :: forall m . Monad m
    => Coin  -- ^ Eligibility threshold
    -> ConduitT (StakeholderId, Coin) Void m RichmenStakes
findRichmenStakes t = CL.fold thresholdInsert mempty
  where
    thresholdInsert
           :: HashMap StakeholderId Coin
           -> (StakeholderId, Coin)
           -> HashMap StakeholderId Coin
    thresholdInsert hm (a, c) = if c >= t then HM.insert a c hm else hm
