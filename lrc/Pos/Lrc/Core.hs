{-# LANGUAGE ScopedTypeVariables #-}

-- | Core logic of LRC.

module Pos.Lrc.Core
       ( findDelegationStakes
       , findRichmenStake
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Universum

import           Pos.Core.Coin       (mkCoin, unsafeAddCoin)
import           Pos.Core.Types      (Coin, StakeholderId)
import           Pos.Lrc.Types       (RichmenSet, RichmenStake)
import           Pos.Util.Iterator   (MonadIterator (..))
import           Pos.Util.Util       (getKeys)


-- | Function helper for delegated richmen. Iterates @Delegate ->
-- [Issuer]@ map and computes the following two sets:
--
-- 1. Old richmen set: those who delegated their own stake and thus
-- lost richmen status.
--
-- 2. Delegates who became richmen.
findDelegationStakes
    :: forall m . MonadIterator (StakeholderId, [StakeholderId]) m
    => (StakeholderId -> m Bool)         -- ^ Check if user is issuer?
    -> (StakeholderId -> m (Maybe Coin)) -- ^ Gets effective stake.
    -> Coin
    -> m (RichmenSet, RichmenStake) -- old richmen, new richmen
findDelegationStakes isIssuer stakeResolver t = do
    (old, new) <- step (mempty, mempty)
    pure (getKeys ((HS.toMap old) `HM.difference` new), new)
  where
    step :: (RichmenSet, RichmenStake)
         -> m (RichmenSet, RichmenStake)
    step richmen = nextItem @(StakeholderId, [StakeholderId]) >>=
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
            foldM (\hs is -> ifM ((>= t) <$> safeBalance is)
                                 (pure $ HS.insert is hs)
                                 (pure hs))
                  old
                  issuers
        pure (oldRichmen, newRichmen)
    safeBalance id = fromMaybe (mkCoin 0) <$> stakeResolver id

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenStake
    :: forall m . MonadIterator (StakeholderId, Coin) m
    => Coin  -- ^ Eligibility threshold
    -> m RichmenStake
findRichmenStake t = step mempty
  where
    step :: RichmenStake -> m RichmenStake
    step hm = nextItem >>=
        maybe (pure hm) (\stake -> step (tryAdd stake hm))
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
