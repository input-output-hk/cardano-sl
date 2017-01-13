{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Richmen part of LRC DB.

module Pos.DB.Lrc.Richmen
       (
         -- * Generalization
         RichmenComponent (..)

         -- * Getters
       , getRichmen

       -- * Operations
       , putRichmen

       -- * Initialization
       , prepareLrcRichmen

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

import           Pos.Binary.Class      (Bi, encodeStrict)
import           Pos.Binary.Types      ()
import           Pos.Constants         (delegationThreshold, updateVoteThreshold)
import           Pos.Context.Class     (WithNodeContext)
import           Pos.Context.Functions (genesisUtxoM)
import           Pos.DB.Class          (MonadDB)
import           Pos.DB.Lrc.Common     (getBi, putBi)
import           Pos.Genesis           (genesisDelegation)
import           Pos.Lrc.Logic         (RichmenType (..), findRichmenPure)
import           Pos.Lrc.Types         (FullRichmenData, Richmen, toRichmen)
import           Pos.Types             (Coin, EpochIndex, StakeholderId, applyCoinPortion,
                                        mkCoin, txOutStake)

----------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------

-- | Class for components that store info about richmen.
class Bi (RichmenData a) => RichmenComponent a where
    -- | Datatype that stored. Consider using 'Richmen' or
    -- 'RichmenStake' or 'FullRichmenData'.
    type RichmenData a :: *
    -- | Converts 'FullRichmenData' to what's need to be saved.
    rcToData :: FullRichmenData -> RichmenData a
    -- | Tag to identify component (short bytestring).
    rcTag :: Proxy a -> ByteString
    -- | Threshold for the richman. Argument is total system stake.
    rcThreshold :: Proxy a -> Coin -> Coin
    -- | Whether to consider delegated stake.
    rcConsiderDelegated :: Proxy a -> Bool

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getRichmen
    :: forall c ssc m.
       (RichmenComponent c, MonadDB ssc m)
    => EpochIndex -> m (Maybe (RichmenData c))
getRichmen = getBi . richmenKey @c

getRichmenP
    :: forall c ssc m.
       (RichmenComponent c, MonadDB ssc m)
    => Proxy c -> EpochIndex -> m (Maybe (RichmenData c))
getRichmenP Proxy = getRichmen @c

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen
    :: forall c ssc m.
       (RichmenComponent c, MonadDB ssc m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmen e = putBi (richmenKey @c e) . (rcToData @c)

putRichmenP
    :: forall c ssc m.
       (RichmenComponent c, MonadDB ssc m)
    => Proxy c -> EpochIndex -> FullRichmenData -> m ()
putRichmenP Proxy = putRichmen @c

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

data SomeRichmenComponent =
    forall c. (RichmenComponent c) =>
              SomeRichmenComponent (Proxy c)

someRichmenComponent
    :: forall c.
       RichmenComponent c
    => SomeRichmenComponent
someRichmenComponent = SomeRichmenComponent (Proxy :: Proxy c)

prepareLrcRichmen
    :: (WithNodeContext ssc m, MonadDB ssc m)
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
    => [(StakeholderId, Coin)] -> Proxy c -> FullRichmenData
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
-- General Keys
----------------------------------------------------------------------------

richmenKey
    :: forall c.
       RichmenComponent c
    => EpochIndex -> ByteString
richmenKey = richmenKeyP proxy
  where
    proxy :: Proxy c
    proxy = Proxy

richmenKeyP
    :: forall c.
       RichmenComponent c
    => Proxy c -> EpochIndex -> ByteString
richmenKeyP proxy e = mconcat ["r/", rcTag proxy, "/", encodeStrict e]

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
    type RichmenData RCSsc = Richmen
    rcToData = toRichmen . snd
    rcTag Proxy = "ssc"
    -- [CSL-93] Use eligibility threshold here.
    rcThreshold Proxy = const (mkCoin 0)
    rcConsiderDelegated Proxy = True

getRichmenSsc :: MonadDB ssc m => EpochIndex -> m (Maybe Richmen)
getRichmenSsc epoch = getRichmen @RCSsc epoch

putRichmenSsc
    :: (MonadDB ssc m)
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
    rcThreshold Proxy = applyCoinPortion updateVoteThreshold
    rcConsiderDelegated Proxy = False

getRichmenUS :: MonadDB ssc m => EpochIndex -> m (Maybe FullRichmenData)
getRichmenUS epoch = getRichmen @RCUs epoch

putRichmenUS
    :: (MonadDB ssc m)
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
    rcThreshold Proxy = applyCoinPortion delegationThreshold
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDB ssc m => EpochIndex -> m (Maybe Richmen)
getRichmenDlg epoch = getRichmen @RCDlg epoch

putRichmenDlg :: (MonadDB ssc m) => EpochIndex -> FullRichmenData -> m ()
putRichmenDlg = putRichmen @RCDlg
