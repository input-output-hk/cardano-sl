{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Pos.Lrc.DB.Richmen
       (
       -- * Initialization
         prepareLrcRichmen

       -- * Concrete instances
       -- ** Ssc
       , RCSsc
       , tryGetSscRichmen

       -- ** US
       , RCUs
       , tryGetUSRichmen

       -- ** Delegation
       , RCDlg
       , tryGetDlgRichmen

       -- * Exported for tests
       , richmenComponents

       , RichmenType (..)
       , findRichmenPure
       ) where

import           Universum

import           Data.Conduit (runConduitPure, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Binary.Core ()
import           Pos.Core (Coin, CoinPortion, HasGenesisBlockVersionData,
                           ProxySKHeavy, StakeholderId, addressHash, applyCoinPortionUp,
                           gdHeavyDelegation, genesisData, sumCoins, unGenesisDelegation,
                           unsafeIntegerToCoin)
import           Pos.Crypto (pskDelegatePk)
import           Pos.DB.Class (MonadDB)
import           Pos.Lrc.Consumer.Delegation (RCDlg, tryGetDlgRichmen)
import           Pos.Lrc.Consumer.Ssc (RCSsc, tryGetSscRichmen)
import           Pos.Lrc.Consumer.Update (RCUs, tryGetUSRichmen)
import           Pos.Lrc.Core (findDelegationStakes, findRichmenStakes)
import           Pos.Lrc.DB.RichmenBase (getRichmenP, putRichmenP)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..), SomeRichmenComponent (..),
                                           someRichmenComponent)
import           Pos.Lrc.Types (FullRichmenData)
import           Pos.Txp.GenesisUtxo (genesisStakes)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen :: MonadDB m => m ()
prepareLrcRichmen = do
    let genesisDistribution = HM.toList genesisStakes
        genesisDelegation   = unGenesisDelegation $
                              gdHeavyDelegation genesisData
    mapM_ (prepareLrcRichmenDo genesisDistribution genesisDelegation)
          richmenComponents
  where
    prepareLrcRichmenDo distr deleg (SomeRichmenComponent proxy) =
        whenNothingM_ (getRichmenP proxy 0) $
            putRichmenP proxy 0 (computeInitial distr deleg proxy)

computeInitial
    :: RichmenComponent c
    => [(StakeholderId, Coin)]              -- ^ Genesis distribution
    -> HashMap StakeholderId ProxySKHeavy   -- ^ Genesis delegation
    -> Proxy c
    -> FullRichmenData
computeInitial initialDistr initialDeleg proxy =
    findRichmenPure
        initialDistr
        (rcInitialThreshold proxy)
        richmenType
  where
    -- A reverse delegation map (keys = delegates, values = issuers).
    -- Delegates must not be issuers so we can simply invert the map
    -- without having to compute a transitive closure.
    revDelegationMap =
        HM.fromListWith (<>) $
        map (\(issuer, delegate) -> (delegate, one issuer)) $
        HM.toList $ map (addressHash . pskDelegatePk) initialDeleg
    richmenType
        | rcConsiderDelegated proxy = RTDelegation revDelegationMap
        | otherwise = RTUsual

----------------------------------------------------------------------------
-- Instances. They are here, because we want to have a DB schema in Pos.DB
----------------------------------------------------------------------------

richmenComponents :: HasGenesisBlockVersionData => [SomeRichmenComponent]
richmenComponents =
    [ someRichmenComponent @RCSsc
    , someRichmenComponent @RCUs
    , someRichmenComponent @RCDlg
    ]

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
