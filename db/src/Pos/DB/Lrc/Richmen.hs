-- | Richmen part of LRC DB.

module Pos.DB.Lrc.Richmen
       (
       -- * Initialization
         prepareLrcRichmen

       -- * Concrete instances
       -- ** Ssc
       , tryGetSscRichmen

       -- ** US
       , tryGetUSRichmen

       -- ** Delegation
       , tryGetDlgRichmen

       , RichmenType (..)
       , findRichmenPure
       ) where

import           Universum

import           Data.Conduit (runConduitPure, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Binary.Class (Bi)
import           Pos.Chain.Delegation (ProxySKHeavy)
import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlockVersionData)
import           Pos.Chain.Genesis (GenesisData, gdHeavyDelegation,
                     unGenesisDelegation)
import           Pos.Chain.Lrc (FullRichmenData, RichmenComponent (..),
                     findDelegationStakes, findRichmenStakes)
import           Pos.Chain.Txp (genesisStakes)
import           Pos.Core (Coin, CoinPortion, StakeholderId, addressHash,
                     applyCoinPortionUp, sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto (pskDelegatePk)
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Lrc.Consumer.Delegation (dlgRichmenComponent,
                     tryGetDlgRichmen)
import           Pos.DB.Lrc.Consumer.Ssc (sscRichmenComponent, tryGetSscRichmen)
import           Pos.DB.Lrc.Consumer.Update (tryGetUSRichmen,
                     updateRichmenComponent)
import           Pos.DB.Lrc.RichmenBase (getRichmen, putRichmen)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen :: MonadDB m => Genesis.Config -> m ()
prepareLrcRichmen genesisConfig = do
    prepareLrcRichmenDo genesisData (sscRichmenComponent genesisBvd)
    prepareLrcRichmenDo genesisData (updateRichmenComponent genesisBvd)
    prepareLrcRichmenDo genesisData (dlgRichmenComponent genesisBvd)
  where
    genesisData = configGenesisData genesisConfig
    genesisBvd  = configBlockVersionData genesisConfig

prepareLrcRichmenDo
    :: (Bi richmenData, MonadDB m)
    => GenesisData
    -> RichmenComponent richmenData
    -> m ()
prepareLrcRichmenDo genesisData rc =
    whenNothingM_ (getRichmen rc 0) . putRichmen rc 0 $ computeInitial
        genesisDistribution
        genesisDelegation
        rc
  where
    genesisDistribution = HM.toList $ genesisStakes genesisData
    genesisDelegation   = unGenesisDelegation $ gdHeavyDelegation genesisData

computeInitial
    :: [(StakeholderId, Coin)]              -- ^ Genesis distribution
    -> HashMap StakeholderId ProxySKHeavy   -- ^ Genesis delegation
    -> RichmenComponent c
    -> FullRichmenData
computeInitial initialDistr initialDeleg rc =
    findRichmenPure
        initialDistr
        (rcInitialThreshold rc)
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
        | rcConsiderDelegated rc = RTDelegation revDelegationMap
        | otherwise = RTUsual

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
