-- | Richmen part of LRC DB.

module Pos.Lrc.DB.Richmen
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
import           Pos.Core (Coin, CoinPortion, ProxySKHeavy, StakeholderId,
                     addressHash, applyCoinPortionUp, gdHeavyDelegation,
                     genesisData, sumCoins, unGenesisDelegation,
                     unsafeIntegerToCoin)
import           Pos.Crypto (pskDelegatePk)
import           Pos.DB.Class (MonadDB)
import           Pos.Lrc.Consumer.Delegation (tryGetDlgRichmen)
import qualified Pos.Lrc.Consumer.Delegation as Dlg (richmenComponent)
import           Pos.Lrc.Consumer.Ssc (tryGetSscRichmen)
import qualified Pos.Lrc.Consumer.Ssc as Ssc (richmenComponent)
import           Pos.Lrc.Consumer.Update (tryGetUSRichmen)
import qualified Pos.Lrc.Consumer.Update as Update (richmenComponent)
import           Pos.Lrc.Core (findDelegationStakes, findRichmenStakes)
import           Pos.Lrc.DB.RichmenBase (getRichmen, putRichmen)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (FullRichmenData)
import           Pos.Txp.GenesisUtxo (genesisStakes)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen :: MonadDB m => m ()
prepareLrcRichmen = do
    prepareLrcRichmenDo Ssc.richmenComponent
    prepareLrcRichmenDo Update.richmenComponent
    prepareLrcRichmenDo Dlg.richmenComponent

prepareLrcRichmenDo
    :: (Bi richmenData, MonadDB m) => RichmenComponent richmenData -> m ()
prepareLrcRichmenDo rc =
    whenNothingM_ (getRichmen rc 0) . putRichmen rc 0 $ computeInitial
        genesisDistribution
        genesisDelegation
        rc
  where
    genesisDistribution = HM.toList genesisStakes
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
