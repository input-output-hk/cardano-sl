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
       , putRichmenSsc

       -- ** US
       , RCUs
       , getRichmenUS
       , putRichmenUS

       -- ** Delegation
       , RCDlg
       , getRichmenDlg
       , putRichmenDlg
       ) where

import           Universum

import           Pos.Binary.Core        ()
import           Pos.Constants          (genesisHeavyDelThd, genesisMpcThd,
                                         genesisUpdateVoteThd)
import           Pos.Context.Class      (WithNodeContext)
import           Pos.Context.Functions  (genesisUtxoM)
import           Pos.DB.Class           (MonadDB)
import           Pos.Genesis            (genesisDelegation)
import           Pos.Lrc.Class          (RichmenComponent (..), SomeRichmenComponent (..),
                                         someRichmenComponent)
import           Pos.Lrc.DB.RichmenBase (getRichmen, getRichmenP, putRichmen, putRichmenP)
import           Pos.Lrc.Logic          (RichmenType (..), findRichmenPure)
import           Pos.Lrc.Types          (FullRichmenData, Richmen, RichmenStake,
                                         toRichmen)
import           Pos.Txp.Core           (TxOutDistribution, txOutStake)
import           Pos.Types              (EpochIndex, applyCoinPortion)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen
    :: (WithNodeContext ssc m, MonadDB m)
    => m ()
prepareLrcRichmen = do
    genesisDistribution <- concatMap txOutStake . toList <$> genesisUtxoM
    mapM_ (prepareLrcRichmenDo genesisDistribution) components
  where
    prepareLrcRichmenDo distr (SomeRichmenComponent proxy) =
        putIfEmpty
            (getRichmenP proxy 0)
            (putRichmenP proxy 0 $ computeInitial distr proxy)

computeInitial
    :: RichmenComponent c
    => TxOutDistribution -> Proxy c -> FullRichmenData
computeInitial initialDistr proxy =
    findRichmenPure initialDistr (rcThreshold proxy) richmenType
  where
    richmenType
        | rcConsiderDelegated proxy = RTDelegation genesisDelegation
        | otherwise = RTUsual

putIfEmpty
    :: forall a m.
       Monad m
    => (m (Maybe a)) -> m () -> m ()
putIfEmpty getter putter = maybe putter (const pass) =<< getter

----------------------------------------------------------------------------
-- Instances. They are here, because we want to have a DB schema in Pos.DB
----------------------------------------------------------------------------

components :: [SomeRichmenComponent]
components = [ someRichmenComponent @RCSsc
             , someRichmenComponent @RCUs
             , someRichmenComponent @RCDlg]

----------------------------------------------------------------------------
-- SSC instance
----------------------------------------------------------------------------

data RCSsc

instance RichmenComponent RCSsc where
    type RichmenData RCSsc = RichmenStake
    rcToData = snd
    rcTag Proxy = "ssc"
    rcThreshold Proxy = applyCoinPortion genesisMpcThd
    rcConsiderDelegated Proxy = True

getRichmenSsc :: MonadDB m => EpochIndex -> m (Maybe RichmenStake)
getRichmenSsc epoch = getRichmen @RCSsc epoch

putRichmenSsc
    :: (MonadDB m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmenSsc = putRichmen @RCSsc

----------------------------------------------------------------------------
-- Update System instance
----------------------------------------------------------------------------

data RCUs

instance RichmenComponent RCUs where
    type RichmenData RCUs = FullRichmenData
    rcToData = identity
    rcTag Proxy = "us"
    rcThreshold Proxy = applyCoinPortion genesisUpdateVoteThd
    rcConsiderDelegated Proxy = True

getRichmenUS :: MonadDB m => EpochIndex -> m (Maybe FullRichmenData)
getRichmenUS epoch = getRichmen @RCUs epoch

putRichmenUS
    :: (MonadDB m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmenUS = putRichmen @RCUs

----------------------------------------------------------------------------
-- Delegation instance
----------------------------------------------------------------------------

data RCDlg

instance RichmenComponent RCDlg where
    type RichmenData RCDlg = Richmen
    rcToData = toRichmen . snd
    rcTag Proxy = "dlg"
    rcThreshold Proxy = applyCoinPortion genesisHeavyDelThd
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDB m => EpochIndex -> m (Maybe Richmen)
getRichmenDlg epoch = getRichmen @RCDlg epoch

putRichmenDlg :: MonadDB m => EpochIndex -> FullRichmenData -> m ()
putRichmenDlg = putRichmen @RCDlg
