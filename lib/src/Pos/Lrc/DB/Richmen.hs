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
       , getRichmenSsc

       -- ** US
       , RCUs
       , getRichmenUS

       -- ** Delegation
       , RCDlg
       , getRichmenDlg

       -- * Exported for tests
       , richmenComponents
       ) where

import           Universum

import qualified Data.HashMap.Strict         as HM

import           Pos.Binary.Core             ()
import           Pos.Context                 (genesisStakes)
import           Pos.Core                    (BlockVersionData (bvdHeavyDelThd), Coin,
                                              EpochIndex, HasConfiguration, ProxySKHeavy,
                                              StakeholderId, addressHash,
                                              gdHeavyDelegation, genesisBlockVersionData,
                                              genesisData, unGenesisDelegation)
import           Pos.Crypto                  (pskDelegatePk)
import           Pos.DB.Class                (MonadDB, MonadDBRead)
import           Pos.Lrc.Class               (RichmenComponent (..),
                                              SomeRichmenComponent (..),
                                              someRichmenComponent)
import           Pos.Lrc.DB.RichmenBase      (getRichmen, getRichmenP, putRichmenP)
import           Pos.Lrc.Logic               (RichmenType (..), findRichmenPure)
import           Pos.Lrc.Types               (FullRichmenData, RichmenSet)
import           Pos.Ssc.RichmenComponent    (RCSsc, getRichmenSsc)
import           Pos.Update.RichmenComponent (RCUs, getRichmenUS)
import           Pos.Util.Util               (getKeys)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen ::
       ( HasConfiguration
       , MonadDB m
       )
    => m ()
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

richmenComponents :: HasConfiguration => [SomeRichmenComponent]
richmenComponents =
    [ someRichmenComponent @RCSsc
    , someRichmenComponent @RCUs
    , someRichmenComponent @RCDlg
    ]

----------------------------------------------------------------------------
-- Delegation instance
----------------------------------------------------------------------------

data RCDlg

instance HasConfiguration => RichmenComponent RCDlg where
    type RichmenData RCDlg = RichmenSet
    rcToData = getKeys . snd
    rcTag Proxy = "dlg"
    rcInitialThreshold Proxy = bvdHeavyDelThd genesisBlockVersionData
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDBRead m => EpochIndex -> m (Maybe RichmenSet)
getRichmenDlg epoch = getRichmen @RCDlg epoch
