{-# LANGUAGE ScopedTypeVariables #-}

-- | Core logic of LRC.

module Pos.Lrc.Core
       ( findDelegationStakes
       , findRichmenStake
       ) where

import           Data.Conduit        (Sink, await)
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Universum

import           Pos.Core.Coin       (mkCoin, unsafeAddCoin)
import           Pos.Core.Types      (Coin, StakeholderId)
import           Pos.Lrc.Types       (RichmenSet, RichmenStake)
import           Pos.Util.Util       (getKeys)


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
    -> Sink (StakeholderId, HashSet StakeholderId)
            m
            (RichmenSet, RichmenStake)             -- ^ Old richmen, new richmen
findDelegationStakes isIssuer stakeResolver t = do
    (old, new) <- step (mempty, mempty)
    pure (getKeys ((HS.toMap old) `HM.difference` new), new)
  where
    step :: (RichmenSet, RichmenStake)
         -> Sink (StakeholderId, HashSet StakeholderId) m (RichmenSet, RichmenStake)
    step richmen = do
        v <- await
        maybe (pure richmen) (onItem richmen >=> step) v
    onItem (old, new) (delegate, issuers) = do
        sumIssuers <-
            foldM (\cr id -> (unsafeAddCoin cr) <$> safeBalance id)
                  (mkCoin 0)
                  issuers
        isIss <- lift $ isIssuer delegate
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
    safeBalance id = fromMaybe (mkCoin 0) <$> lift (stakeResolver id)

-- | Find nodes which have at least 'eligibility threshold' coins.
findRichmenStake
    :: forall m . Monad m
    => Coin  -- ^ Eligibility threshold
    -> Sink (StakeholderId,Coin) m RichmenStake
findRichmenStake t = CL.fold tryAdd mempty
  where
    tryAdd :: HashMap StakeholderId Coin
           -> (StakeholderId, Coin)
           -> HashMap StakeholderId Coin
    -- Adding coins here should be safe because in utxo we're not supposed to
    -- ever have more coins than the total possible number of coins, and the
    -- total possible number of coins is less than Word64
    tryAdd hm (a, c) = if c >= t then HM.insert a c hm else hm
