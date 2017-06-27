{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
       ) where

import           Universum

import qualified Data.HashMap.Strict         as HM
import qualified Ether

import           Pos.Binary.Core             ()
import           Pos.Constants               (genesisHeavyDelThd)
import           Pos.Context                 (GenesisStakes, genesisStakesM)
import           Pos.Core                    (EpochIndex, applyCoinPortion)
import           Pos.DB.Class                (MonadDB, MonadDBRead)
import           Pos.Genesis                 (genesisDelegation)
import           Pos.Lrc.Class               (RichmenComponent (..),
                                              SomeRichmenComponent (..),
                                              someRichmenComponent)
import           Pos.Lrc.DB.RichmenBase      (getRichmen, getRichmenP, putRichmenP)
import           Pos.Lrc.Logic               (RichmenType (..), findRichmenPure)
import           Pos.Lrc.Types               (FullRichmenData, Richmen, toRichmen)
import           Pos.Ssc.RichmenComponent    (RCSsc, getRichmenSsc)
import           Pos.Txp.Core                (TxOutDistribution)
import           Pos.Update.RichmenComponent (RCUs, getRichmenUS)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen
    :: (Ether.MonadReader' GenesisStakes m, MonadDB m)
    => m ()
prepareLrcRichmen = do
    genesisDistribution <- HM.toList <$> genesisStakesM
    mapM_ (prepareLrcRichmenDo genesisDistribution) components
  where
    prepareLrcRichmenDo distr (SomeRichmenComponent proxy) =
        whenNothingM_ (getRichmenP proxy 0) $
            putRichmenP proxy 0 (computeInitial distr proxy)

computeInitial
    :: RichmenComponent c
    => TxOutDistribution -> Proxy c -> FullRichmenData
computeInitial initialDistr proxy =
    findRichmenPure
        initialDistr
        (applyCoinPortion (rcInitialThreshold proxy))
        richmenType
  where
    richmenType
        | rcConsiderDelegated proxy = RTDelegation genesisDelegation
        | otherwise = RTUsual

----------------------------------------------------------------------------
-- Instances. They are here, because we want to have a DB schema in Pos.DB
----------------------------------------------------------------------------

components :: [SomeRichmenComponent]
components = [ someRichmenComponent @RCSsc
             , someRichmenComponent @RCUs
             , someRichmenComponent @RCDlg]

----------------------------------------------------------------------------
-- Delegation instance
----------------------------------------------------------------------------

data RCDlg

instance RichmenComponent RCDlg where
    type RichmenData RCDlg = Richmen
    rcToData = toRichmen . snd
    rcTag Proxy = "dlg"
    rcInitialThreshold Proxy = genesisHeavyDelThd
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDBRead m => EpochIndex -> m (Maybe Richmen)
getRichmenDlg epoch = getRichmen @RCDlg epoch
